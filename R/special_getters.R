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
    f <- read_lines(system.file("extdata/verbs",paste0(lang,"_",tolower(groupname),".txt"),package="phdR"))
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


#' Hae l2-aineistoryhmien kollokaatteja ja muodosta tarvittavat taulukot yms.
#'
#' @importFrom pbapply pbapply
#' @importFrom reshape melt
#' @import ggplot2
#' @export

GetL2Collocates <- function(){
    #1. Luo alijoukot listaksi. Lisää myös muuttuja, joka mittaa ajanilmauksen
    # mahdollista toistumista samassa lauseessa ennen tai jälkeen tarkasteltavan
    # ilmauksen
    if(is.null(researchdata$L2data)){
        researchdata$L2data <- setNames(lapply(langlist,function(l)return( setNames(lapply(c("L2a","L2b"),function(g,l2){
                                                 locs <- c("S1","S4")
                                                 locs_data <- setNames(lapply(locs,function(thisloc){
                                                     ss <- subset(d,lang==l2&group==g&location==thisloc)
                                                     #Poista kollokaattien tunnistamista häiritsevät joissain araneum-tapauksissa esiintyvät doc-tägit
                                                     ss$sent <- gsub("<doc>","",ss$sent)
                                                     #Tutki, missä lauseissa jokin ko. positionaalisen aineistoryhmän ajanilmaus toistuu
                                                     ss$rep <- "no"
                                                     pat <- ifelse(g=="L2a",researchdata$patterns[[l2]]$wd$pat,researchdata$patterns[[l2]]$month$pat)
                                                     #JOKO niin, että toisto ennen tai niin että toisto varsinaisen ilmauksen jälkeen
                                                     ss$rep[which(grepl(paste0(pat, ".*", gsub("[<>]","",pat)),ss$sent,ignore.case=T) |
                                                                  grepl(paste0(gsub("[<>]","",pat),".*", pat),ss$sent,ignore.case=T))] <- "yes"
                                                     collocates <- SimpleCollocations(ss$sent,pat,2)
                                                     ss$colloc.left1 <- collocates$left[[1]][2:length(collocates$left[[1]])]
                                                     ss$colloc.left2 <- collocates$left[[2]][2:length(collocates$left[[2]])]
                                                     ss$colloc.right1 <- collocates$right[[1]][2:length(collocates$right[[1]])]
                                                     ss$colloc.right2 <- collocates$right[[2]][2:length(collocates$right[[2]])]
                                                     return(ss)
                                                 }),locs)

                                                 return(locs_data)
                                                 }
                                                ,l2=l),c("a","b")))), researchdata$langs)
    }

    l2areps.fi <- xtabs(~ colloc.left1 + rep, researchdata$L2data$fi$a$S1)["---",]
    l2breps.fi <- xtabs(~ colloc.left1 + rep, researchdata$L2data$fi$b$S1)["---",]
    l2areps.ru <- xtabs(~ colloc.left1 + rep, researchdata$L2data$ru$a$S1)["---",]
    l2breps.ru <- xtabs(~ colloc.left1 + rep, researchdata$L2data$ru$b$S1)["---",]

    fi <- melt(matrix(c(as.vector(l2areps.fi),as.vector(l2breps.fi)),byrow=T,nrow=2,dimnames=list(c("l2a","l2b"),c("no","yes"))))
    fi$lang <- "fi"
    ru <- melt(matrix(c(as.vector(l2areps.ru),as.vector(l2breps.ru)),byrow=T,nrow=2,dimnames=list(c("l2a","l2b"),c("no","yes"))))
    ru$lang <- "ru"

    noncolstats <- rbind(fi,ru)
    colnames(noncolstats) <- c("group","repeated","value","lang")
    totals <- list("fi"=list("l2a"=sum(l2areps.fi),"l2b"=sum(l2breps.fi)), 
                   "ru"=list("l2a"=sum(l2areps.ru),"l2b"=sum(l2breps.ru)))
    noncolstats$pr <-  apply(noncolstats,1,function(myrow,totals) return (100*(as.integer(myrow[["value"]]) / totals[[myrow[["lang"]]]][[myrow["group"]]])),totals=totals)
    researchdata$L2data$noncolplot <- ggplot(noncolstats,aes(x=lang, y=pr, fill=repeated)) + geom_bar(stat="identity") + facet_wrap(~ group) + theme_bw() + scale_fill_grey(start = 0.3, end = .7) 
}

#' Hakee monia l1a-aineistoon ja keskisijaintiin liittyviä tietoja erityisesti koskien ns. julkaisu- / luomisverbejä
#' @importFrom ggplot2 ggplot aes geom_bar facet_grid
#' @importFrom reshape melt
#' @export
GetL1aDataForS2S3 <- function(){

    if(is.null(researchdata$L1a_data_for_s2s3)){
    LoadData()
    researchdata$L1a_data_for_s2s3 <- list()
    researchdata$L1a_data_for_s2s3$l1a <- list("fi"=subset(d, lang=="fi" & group=="L1a"),"ru"=subset(d, lang=="ru" & group=="L1a"))
    researchdata$L1a_data_for_s2s3$publishverbs <- setNames(lapply(researchdata$langs,
                       function(lang,l1a,verbs){
                           x<-sort(table(l1a[[lang]]$headverb),decreasing=T)
                           d <- as.data.frame(x[verbs[[lang]]])
                           colnames(d) <- c("verbi","frekvenssi")
                           return(d)
                       },
                       l1a=researchdata$L1a_data_for_s2s3$l1a,verbs=list("fi"=GetPublishVerbs("fi","L1a"),"ru"=GetPublishVerbs("ru","L1a"))),researchdata$langs)

    # merkkaa julkaisuverbien läsnäolo kaikkiin g.df-listan datoihin
    for(lang in researchdata$langs){
        researchdata$L1a_data_for_s2s3$l1a[[lang]]$hasPverb <- "no"
        researchdata$L1a_data_for_s2s3$l1a[[lang]]$hasPverb[researchdata$L1a_data_for_s2s3$l1a[[lang]]$headverb %in% researchdata$L1a_data_for_s2s3$publishverbs[[lang]]$verbi] <- "yes"
        for (gname in names(researchdata$g.df)){
            for(loc in names(researchdata$g.df[[gname]])){
                researchdata$g.df[[gname]][[loc]][[lang]]$hasPverb <- "no"
                researchdata$g.df[[gname]][[loc]][[lang]]$hasPverb[researchdata$g.df[[gname]][[loc]][[lang]]$headverb %in% researchdata$L1a_data_for_s2s3$publishverbs[[lang]]$verbi] <- "yes"
            }
        }
    }

    researchdata$L1a_data_for_s2s3$all.indicators <- list("ru"=subset(researchdata$L1a_data_for_s2s3$l1a$ru, subjtype=="long"&hasPverb=="yes"&corpustype=="press"),
                          "fi"=subset(researchdata$L1a_data_for_s2s3$l1a$fi, subjtype=="long"&hasPverb=="yes"&corpustype=="press"))

    researchdata$L1a_data_for_s2s3$all.indicators$props <- lapply(researchdata$L1a_data_for_s2s3$all.indicators,function(x)return(100*prop.table(table(x$location3))))
    researchdata$L1a_data_for_s2s3$all.indicators$plot  <- ggplot(melt(matrix(c(researchdata$L1a_data_for_s2s3$all.indicators$props$fi,researchdata$L1a_data_for_s2s3$all.indicators$props$ru),nrow=2, byrow=T,dimnames=list(c("fi","ru"),c("S1","S2/S3","S4"))),varnames=c("lang","location")),
                            aes(x=location,y=value)) +  geom_bar(stat="identity", width=.3) + facet_grid(lang ~.)

    for(lang in researchdata$langs){
        researchdata$L1a_data_for_s2s3$l1a[[lang]]$corpustype <- as.factor(researchdata$L1a_data_for_s2s3$l1a[[lang]]$corpustype)
        researchdata$L1a_data_for_s2s3$l1a[[lang]]$subtype <- as.factor(researchdata$L1a_data_for_s2s3$l1a[[lang]]$subjtype)
        researchdata$L1a_data_for_s2s3$l1a[[lang]]$hasPverb <- as.factor(researchdata$L1a_data_for_s2s3$l1a[[lang]]$hasPverb)
    }
    researchdata$L1a_data_for_s2s3$dataframe <- rbind(researchdata$L1a_data_for_s2s3$l1a$fi,researchdata$L1a_data_for_s2s3$l1a$ru)

    researchdata$L1a_data_for_s2s3$l1a.hasPverb.proptabs <- setNames(lapply(researchdata$langs,function(lang,l1a) return(100*prop.table(xtabs(~location3, data=l1a[[lang]],subset=hasPverb=="yes"))), l1a=researchdata$L1a_data_for_s2s3$l1a),researchdata$langs)
    }

}


#' Laske kehyksiseen resultatiiviseen funktioon liittyviä tilastoja erityisesti siitä, missä lauseissa on mukana numeraali
#' @export

GetKehRes <- function(){
    if (is.null(researchdata$keh.res)){
        researchdata$keh.res <- setNames(lapply(researchdata$langs,function(thislang){
                             numpat <- ifelse(thislang=="fi","\\[NUM>>","\\[M>>")
                             all.cases <- subset(d, funct=="res.keh"&lang==thislang)
                             numeric.cases <- all.cases[(which(grepl(numpat,all.cases$feats))),]
                             non.numeric.cases <- all.cases[(which(!grepl(numpat,all.cases$feats))),]
                             return(list("all"=all.cases,"number"=numeric.cases,"nonumber"=non.numeric.cases))
                             }),researchdata$langs)
        researchdata$keh.res$all <- rbind(researchdata$keh.res$fi$all,researchdata$keh.res$ru$all)
    }
}




#' Laske s1-sijainnin keskiarvoja. Kerää myös functtabs-taulukko eri semanttisten funktioiden jakautumisesta eri sijainteihin
#' @export

GetS1Means <- function(){
    countmean <- function(x, lang, functtabs){
        pt <- prop.table(functtabs[lang,,x])*100
        retval <- pt["S1"]
        names(retval) <- x
        return(retval)
    }

    if (is.null(researchdata$rus1means)){
        researchdata$functtabs <- xtabs(~lang + location + funct, data=d)
        functs <- unique(d$funct)

        researchdata$rus1means <- sapply(functs, countmean,lang="ru",functtabs=researchdata$functtabs)
        researchdata$fis1means <- sapply(functs, countmean,lang="fi",functtabs=researchdata$functtabs)

        sapply(unique(d$funct), function(x){pt <- prop.table(researchdata$functtabs["fi",,x])*100;return(pt["S1"])})
        researchdata$yint1 <- mean(researchdata$fis1means)
        researchdata$yint2 <- mean(researchdata$rus1means)
    }
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
