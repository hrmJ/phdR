#' Tähän tiedostoon on listattu funktioita, joiden tehtävä on, mahdollisesti
#' aika kertakäyttöisesti, hakea dataa tietyssä muodossa tai tietyllä tavalla
#' filtteröitynä isosta d-dataframesta

#' Hae adverbien osuus tietyssä sijainnissa
#' @param lang kieli
#' @param total dataframe, josta haetaan
#' @param location sijainti, josta haetaan
#' @export
GetAdvShare  <- function(x, lang, total, location){
    share  <-  nrow(total[total$group==x&total$lang==lang&total$location3=="S2/S3",])/nrow(total[total$group==x&total$lang==lang,])
    names(share) <- x
    return(share)
} 



#' Hae kaikki yhden kielen "julkaisuverbeiksi" luokitellut verbit pakettiin sisällytestä datatiedostosta
#' @param lang kieli (fi/ru)
#' @param groupname aineistoryhmä, jonka perusteella verbejä etsitään
#' @importFrom readr read_lines
#' @export
GetPublishVerbs <- function(lang, groupname){
    f <- read_lines(system.file("extdata/verbs",paste0(lang,"_",tolower(groupname),".txt"),package="phdR2"))
    f<-f[grepl("^!!",f)]
    return (sub("!!","",f))
}




#' Palauttaa listan, jossa inputtina annettu dataframe on subsetattu sen mukaan, mitä pronomineja siinä on käyttäjän määrittelemässä roolissa (esim. subjektina)
#'
#' @param d dataframe, jota filtteröidään
#' @param filterby sen sarakkeen nimi, josta pronomineja etsitään
#' 
#' @export

FilterPronouns <- function(d=data.frame(),filterby=character()){
    mina <- c("minä","mä","я","мы")
    sina <- c("sä","sinä","ты","вы")
    han <- c("hän","se","это","этот","тот","то","оно","он","она","они")
    joku <- c("joku","кто-то","кто-нибудь")

    return(list(mina=d[d[[filterby]] %in% mina,],
                sina=d[d[[filterby]] %in% sina,],
                han=d[d[[filterby]] %in% han,],
                joku=d[d[[filterby]] %in% joku,],
                muu=d[!d[[filterby]] %in% c(mina,sina,han,joku),]))
}




#' Pyrkii rajaamaan S4-aineistoa niin, että siitä erotellaan tapauksia, jotka
#' *ovat* fokaalisia tapauksista, jotka eivät ole.
#' 
#' @importFrom reshape melt
#' @import ggplot2
#' @importFrom pbapply pblapply
#' @export 

GetNonFocalTest <- function(){

    if(is.null(researchdata$nonfocaltest)){
        ru.deict <- subset(d,morph=="deict.ADV"&lang=="ru")
        cat("Searching for adv + place patterns in the data...","\n")
        #venäjän partisiippitapaukset?
        nonfocaltest <- subset(d,headverbtense=="past"&funct=="sim"&(morph=="deict.ADV" | pos==1)&!group %in% c("L8a","L8b","L8c","L1c"))
        cat("Lasketaan ei-fokaalisen tilastollisen mallin yhteyteen kollokaatteja...","\n")
        colloc <- SimpleCollocations(nonfocaltest$sent,"<([a-öA-Öа-яА-Я]+)>",2)
        leftcollocates <- colloc$left[[1]][2:length(colloc$left[[1]])]
        secondleftcollocates <- colloc$left[[2]][2:length(colloc$left[[2]])]

        nonfocaltest$leftcollocate <- leftcollocates
        nonfocaltest$secondleftcollocate <- secondleftcollocates
        nonfocaltest$followed_by_locative <- pbapply(nonfocaltest,1,MarkIfHasPlace,direction="next")
        nonfocaltest$preceded_by_locative <- pbapply(nonfocaltest,1,MarkIfHasPlace,direction="previous")
        #KORJATAAN manuaalisesti virheellisiä tapauksia:
        nonfocaltest$followed_by_locative[grepl("> (illalla|aamulla|aamuyöllä|aamupäivällä|iltapäivällä)",nonfocaltest$sent)]  <- "no"
        #venäjässä в том числе -tapaukset ja по + datiivi -tapaukset
        nonfocaltest$preceded_by_locative[nonfocaltest$lang=="ru"& (nonfocaltest$leftcollocate %in% c("числе") | nonfocaltest$secondleftcollocate %in% c("по"))] <- "no"
        #Tapaukset, jotka oikeasti alkavat isolla kirjaimella eli joiden edeltävä lokatiivi ei oikeasti kuulu edes samaan lauseeseen
        nonfocaltest$preceded_by_locative[nonfocaltest$lang=="ru"& grepl('(<Во|Вчера|ТТС"еще)',nonfocaltest$sent)] <- "no"
        #Älä katso edeltäviksi paikanilmauksia tapauksia, joissa edeltävä paikanilmaus on erotettu pilkulla 
        #ESIM: [4] "В июне 2014 года мы презентовали проект на международной цифровой конференции Futur-en-Seine в Париже, <в> октябре – стали участниками Санкт-Петербургского Международного Медиа Форума, а в ноябре мы были в Дублине на крупнейшей IT-конференции в мире Web Summit."
        nonfocaltest$preceded_by_locative[grepl(', <',nonfocaltest$sent)] <- "no"

        nonfocaltest$locative_neighbour <- "none"
        nonfocaltest$locative_neighbour[nonfocaltest$followed_by_locative=="yes"] <- "next"
        nonfocaltest$locative_neighbour[nonfocaltest$preceded_by_locative=="yes"] <- "previous"
        nonfocaltest$haslocative_neighbour <- "no"
        nonfocaltest$haslocative_neighbour[nonfocaltest$locative_neighbour!="none"] <- "yes"
        nonfocaltest$locative_neighbour_and_press = "none"
        nonfocaltest$locative_neighbour_and_press <- paste(nonfocaltest$locative_neighbour,nonfocaltest$corpustype)
        nonfoctabs <- xtabs(~lang+locative_neighbour,nonfocaltest)
        #S4-aseman ja lokatiivinaapurin yhteyttä tutkiva kuvio
        cats <- unique(nonfocaltest$locative_neighbour)
        tables <- xtabs(~lang+location3+locative_neighbour,nonfocaltest)
        locations <- melt(setNames(lapply(cats,function(x,tables)return(prop.table(tables[,,x],1)*100),tables=tables),cats))
        locations$L1 <- factor(locations$L1,levels=c("none","next","previous"))
        locations.plot <- ggplot(subset(locations,location3=="S4"),aes(x=lang,y=value)) + geom_bar(stat="identity") + facet_grid(~L1) + theme_bw() + scale_fill_grey(start = 0.3, end = .9,guide=F) 
        researchdata$nonfocaltest <- list(alldata=nonfocaltest, tabs=nonfoctabs, loc.plot=locations.plot)
    }
}
