
#' Muodostaa karsitun, nopeasti viitattavan version tutkimusdatasta
#' 
#' @param customdataset jos annetaan lähtökohtana oleva data frame argumenttina
#' 
#' @importFrom dplyr select  %>% filter mutate mutate_if
#' @importFrom tibble rowid_to_column
#' @importFrom pbapply pbapply
#' 
#' @export

GetD <- function(rawdata){
    rawdata <- rawdata %>% filter(!duplicated(sent))
    rawdata$ID <- seq.int(nrow(rawdata))
    cat("Määritellään numeraalien läsnäolo -muuttujaa, tämä voi kestää hieman...")
    rawdata$isnumeric <- pbapply(rawdata,1,DoesThisRowHaveNumeral)
    sents <- rawdata %>% as_tibble %>% select(ID, sent,sourcetext,lang)
    d <- rawdata %>% as_tibble %>% filter(!group %in% c("L9b")) %>% 
        mutate(funct=as.character(funct)) %>% 
        mutate(funct=case_when(group=="L6a" ~ "dist" ,TRUE ~ funct))  %>% 
        mutate(morph=as.character(morph))  %>% 
        mutate(morph=case_when(ref=="anaf" ~ "anaf",TRUE ~ morph))  %>% 
        select(ID, lang, group,funct,morph,location3,location,
               isnumeric, pos, ref, corpustype, subjtype, objtype,
               headverb, headverbfeat, subjpos, objpos,
               firstpos, firsttoken, subjlength, subjlength2,
               objlength,
               subjlemma, objlemma,
               clausestatus, sentid) %>% 
        mutate_if(is.factor,as.character) 


    save(d, file="~/workprojects/phdR2/data/d.rda")
    save(sents, file="~/drive/work/tutkimus/data/phd_manuscript_data/data/sents.rda")

} 


#' Just a quick shortcut for joining sentences
#' 
#' @importFrom dplyr  %>% left_join
#' 
#' @export

AddSents <- function(mydf){
    load("~/phd_data/data/sents.rda")
    return (left_join(mydf,sents,by=c("ID","lang")))
}


#' Lisää uuden ryhmän suoraan json-tiedostosta
#' 
#' @param path plku jsontiedostoon
#' @param lang kieli
#' 
#' @importFrom dplyr %>% mutate as_tibble
#' @importFrom pbapply pbapply
#' @export
AddGroupFromJson  <- function(path, lang){
    rawd <- ProcessJson(path, lang) 
    if(nrow(rawd)>0){
            rawd %>% 
            FormatGroupName  %>% 
            FilterOut -> filtered
        if(nrow(filtered)>0){
            filtered %>% 
            AddFixGroups  %>% 
            FixGroupsFi %>% 
            FixGroupsRu  %>% 
            AddVariables  %>% 
            FixVariables -> processed
            if(nrow(processed %>% filter(is.na(processed$funct)))>0){
                cat("NO DATA after processing:  ", path)
            }
            else{
                return (processed)
            }
        }
        else{
            cat("File ", path, "filtered out")
        }
    }
    else{
        cat("File ", path, " empty or invalid")
    }
}

#' Lisää kokonaisen kansiollisen json-tiedostoja dataan
#' 
#' @param path polku kansioon
#' @param thislang kieli
#' 
#' @importFrom pbapply pblapply
#' @importFrom dplyr  %>% 
#' @export
#' 

AddFolder <- function(path, thislang){
    paths <- paste0(path,list.files(path))
    folder <- pblapply(paths, AddGroupFromJson, lang=thislang)
    return(folder)
}



#' Luo nopeamman viittauksen mahdollistavia shortcut-tibblejä
#' 
#' @importFrom pbapply pblapply
#' @export

CreateGtabs <- function(){

        #Ristiintaulukointeja prosenttiosuuksilla aineistoryhmittäin ja koko aineiston laajuisesti
        gtabs <- lapply(unique(as.character(d$group)),function(x,df) return(list(counts=xtabs(~lang + location3, df,group==x), props=100*prop.table(xtabs(~lang + location3, df,group==x),1))),df=d)
        names(gtabs) <- unique(as.character(d$group))
        gtabs$all <- list(props=100*prop.table(xtabs(~lang + location3, d),1), counts=xtabs(~lang + location3, d))

        #Aineistoryhmäkohtaisia sijainneittain jaoteltuja dataframejä
        g.df <- GroupSubsets(unique(d$group))
        g.df.simp <- setNames(pblapply(unique(d$group),function(x,v1df){ tmp=subset(v1df,group==x);tmp$group<-as.character(tmp$group); return(tmp)},v1df=d),unique(d$group))
        save(gtabs,file="~/workprojects/phdR2/data/gtabs.rda")
        save(g.df,file="~/workprojects/phdR2/data/g.df.rda")
        save(g.df.simp,file="~/workprojects/phdR2/data/g.df.simp.rda")
}

#' Luo koko tutkimusdatasta aineistoryhmäkohtaisia alijoukkoja. 
#' 
#' @param groupname lista ryhmistä, joista alijoukot luodaan
#' 
#' @importFrom pbapply pblapply
#' @export

GroupSubsets <- function(groupnames){
    tmp <- setNames(pblapply(groupnames,function(x){
                 return(list("alku"=list(
                    "fi"=d[d$lang=="fi"&d$group==x&d$location3=="S1",],
                    "ru"=d[d$lang=="ru"&d$group==x&d$location3=="S1",]),
                 "keski"=list(
                    "fi"=d[d$lang=="fi"&d$group==x&d$location3=="S2/S3",],
                    "ru"=d[d$lang=="ru"&d$group==x&d$location3=="S2/S3",]),
                 "loppu"=list(
                    "fi"=d[d$lang=="fi"&d$group==x&d$location3=="S4",],
                    "ru"=d[d$lang=="ru"&d$group==x&d$location3=="S4",])
                    ))}),groupnames)
    return(tmp)
}


#' Laske s1-sijainnin keskiarvoja. Kerää myös functtabs-taulukko eri semanttisten funktioiden jakautumisesta eri sijainteihin
#' 
#' @export

GetS1Means <- function(){
    countmean <- function(x, lang, functtabs){
        pt <- prop.table(functtabs[lang,,x])*100
        retval <- pt["S1"]
        names(retval) <- x
        return(retval)
    }

    functtabs <- xtabs(~lang + location + funct, data=d)
    functs <- unique(d$funct)

    rus1means <- sapply(functs, countmean,lang="ru",functtabs=functtabs)
    fis1means <- sapply(functs, countmean,lang="fi",functtabs=functtabs)

    sapply(unique(d$funct), function(x){pt <- prop.table(functtabs["fi",,x])*100;return(pt["S1"])})
    yint1 <- mean(fis1means)
    yint2 <- mean(rus1means)
    save(functtabs,file="~/workprojects/phdR2/data/functtabs.rda")
    save(rus1means,file="~/workprojects/phdR2/data/rus1means.rda")
    save(yint1,file="~/workprojects/phdR2/data/yint1.rda")
    save(yint2,file="~/workprojects/phdR2/data/yint2.rda")
}



#' Hae jokaisesta aineistoryhmästä ne lauseet, joissa on numeraali
#' @export
#' @importFrom pbapply pbsapply

GetNumericCasesInGroups <- function(){
    numeric.cases.in.groups <- setNames(lapply(langlist,function(lang){
        group.ratios <- sort(pbsapply(unique(d$group),GiveNumericals,mydf=d,lang=lang),decreasing=T)
        top <- group.ratios[group.ratios>ifelse(lang=="fi",31,31)&group.ratios!=50]
        #Poistetaan E1c, koska siinä S1-osuus on ylipäätään niin pieni
        top <-top[which(names(top)!="E1c")]
        return(list(all=group.ratios,top=top))
     }),langlist)
    save(numeric.cases.in.groups, file="~/workprojects/phdR2/data/numeric.cases.in.groups.rda")
}

#' Tallentaa joukon valmiita sanalistoja tms.
#' 
#' @export
GetPatterns <- function(){
    patterns = list()
    patterns$fi <- list(wd=list(),months=list())
    patterns$ru <- list(wd=list(),months=list())

    patterns$fi$wd$words <- c("<maanantaina>","<tiistaina>","<keskiviikkona>","<torstaina>","<perjantaina>","<lauantaina>","<sunnuntaina>")
    patterns$fi$wd$pat <- paste0("(",paste(patterns$fi$wd$words,collapse="|"),")")
    patterns$ru$wd$words <- c(paste("<в>",c("понедельник","вторник","среду","четверг","пятницу","субботу","воскресенье")),"<во> вторник")
    patterns$ru$wd$pat <- paste0("(",paste(patterns$ru$wd$words,collapse="|"),")")

    patterns$fi$months$words <- c("<tammikuussa>","<helmikuussa>", "<maaliskuussa>", "<huhtikuussa>", "<toukokuussa>", "<kesäkuussa>","<heinäkuussa>","<elokuussa>","<syyskuussa>","<lokakuussa>","<marraskuussa>","<joulukuussa>")
    patterns$fi$months$partitive <- c("tammikuuta","helmikuuta", "maaliskuuta", "huhtikuuta", "toukokuuta", "kesäkuuta","heinäkuuta","elokuuta","syyskuuta","lokakuuta","marraskuuta","joulukuuta")
    patterns$fi$months$genetive <- c("tammikuun","helmikuun", "maaliskuun", "huhtikuun", "toukokuun", "kesäkuun","heinäkuun","elokuun","syyskuun","lokakuun","marraskuun","joulukuun")
    patterns$fi$months$pat <- paste0("(",paste(patterns$fi$months$words,collapse="|"),")")
    patterns$ru$months$words <- c(paste("<в>",c("январе","феврале","марте","апреле","мае","июне","июле","августе","сентябре","октябре","ноябре","декабре")))
    patterns$ru$months$pat <- paste0("(",paste(patterns$ru$months$words,collapse="|"),")")
    patterns$ru$months$partitive <- c(c("января","февраля","марта","апреля","мая","июня","июля","августа","сентября","октября","ноября","декабря"))
    save(patterns,file="~/workprojects/phdR2/data/patterns.rda")
}


#' Hae l2-aineistoryhmien kollokaatteja ja muodosta tarvittavat taulukot yms.
#'
#' @importFrom pbapply pbapply
#' @importFrom reshape melt
#' @importFrom dplyr  %>% filter select
#' @import ggplot2
#' @export

GetL2Collocates <- function(){
    #1. Luo alijoukot listaksi. Lisää myös muuttuja, joka mittaa ajanilmauksen
    # mahdollista toistumista samassa lauseessa ennen tai jälkeen tarkasteltavan
    # ilmauksen
    load("~/phd_data/data/sents.rda")
    L2data <- setNames(lapply(langlist,function(l)return( 
     setNames(lapply(c("L2a","L2b"),function(g,l2){
         locs <- c("S1","S4")
         locs_data <- setNames(lapply(locs,function(thisloc){
             withsents <- d %>% left_join(sents,by=c("ID","lang"))
             ss <- subset(withsents,lang==l2&group==g&location==thisloc)
             #Poista kollokaattien tunnistamista häiritsevät joissain araneum-tapauksissa esiintyvät doc-tägit
             ss$sent <- gsub("<doc>","",ss$sent)
             #Tutki, missä lauseissa jokin ko. positionaalisen aineistoryhmän ajanilmaus toistuu
             ss$rep <- "no"
             pat <- ifelse(g=="L2a",patterns[[l2]]$wd$pat,patterns[[l2]]$month$pat)
             #JOKO niin, että toisto ennen tai niin että toisto varsinaisen ilmauksen jälkeen
             ss$rep[which(grepl(paste0(pat, ".*", gsub("[<>]","",pat)),ss$sent,ignore.case=T) |
                          grepl(paste0(gsub("[<>]","",pat),".*", pat),ss$sent,ignore.case=T))] <- "yes"
             collocates <- SimpleCollocations(ss$sent,pat,2)
             ss$colloc.left1 <- collocates$left[[1]][2:length(collocates$left[[1]])]
             ss$colloc.left2 <- collocates$left[[2]][2:length(collocates$left[[2]])]
             ss$colloc.right1 <- collocates$right[[1]][2:length(collocates$right[[1]])]
             ss$colloc.right2 <- collocates$right[[2]][2:length(collocates$right[[2]])]
             ss  <- ss %>% select(ID, colloc.left1, colloc.left2, colloc.right1, colloc.right2, rep, subjlength2)
             return(ss)
         }),locs)

         return(locs_data)
         }
        ,l2=l),c("a","b")))), langlist)

    l2areps.fi <- xtabs(~ colloc.left1 + rep, L2data$fi$a$S1)["---",]
    l2breps.fi <- xtabs(~ colloc.left1 + rep, L2data$fi$b$S1)["---",]
    l2areps.ru <- xtabs(~ colloc.left1 + rep, L2data$ru$a$S1)["---",]
    l2breps.ru <- xtabs(~ colloc.left1 + rep, L2data$ru$b$S1)["---",]

    #fi <- melt(matrix(c(as.vector(l2areps.fi),as.vector(l2breps.fi)),byrow=T,nrow=2,dimnames=list(c("l2a","l2b"),c("no","yes"))))
    #fi$lang <- "fi"
    #ru <- melt(matrix(c(as.vector(l2areps.ru),as.vector(l2breps.ru)),byrow=T,nrow=2,dimnames=list(c("l2a","l2b"),c("no","yes"))))
    #ru$lang <- "ru"

    #noncolstats <- rbind(fi,ru)
    #colnames(noncolstats) <- c("group","repeated","value","lang")
    #totals <- list("fi"=list("l2a"=sum(l2areps.fi),"l2b"=sum(l2breps.fi)), 
    #               "ru"=list("l2a"=sum(l2areps.ru),"l2b"=sum(l2breps.ru)))
    #noncolstats$pr <-  apply(noncolstats,1,function(myrow,totals) return (100*(as.integer(myrow[["value"]]) / totals[[myrow[["lang"]]]][[myrow["group"]]])),totals=totals)
    #L2data$noncolplot <- ggplot(noncolstats,aes(x=lang, y=pr, fill=repeated)) + geom_bar(stat="identity") + facet_wrap(~ group) + theme_bw() + scale_fill_grey(start = 0.3, end = .7) 

    save(L2data, file="~/workprojects/phdR2/data/L2data.rda")
    return(L2data)
}

#' Hakee monia l1a-aineistoon ja keskisijaintiin liittyviä tietoja erityisesti koskien ns. julkaisu- / luomisverbejä
#' @importFrom ggplot2 ggplot aes geom_bar facet_grid
#' @importFrom reshape melt
#' @export
GetL1aDataForS2S3 <- function(){

    L1a_data_for_s2s3 <- list()
    L1a_data_for_s2s3$l1a <- list("fi"=subset(d, lang=="fi" & group=="L1a"),"ru"=subset(d, lang=="ru" & group=="L1a"))
    L1a_data_for_s2s3$publishverbs <- setNames(lapply(langlist,
                       function(lang,l1a,verbs){
                           x<-sort(table(l1a[[lang]]$headverb),decreasing=T)
                           d <- as.data.frame(x[verbs[[lang]]])
                           colnames(d) <- c("verbi","frekvenssi")
                           return(d)
                       },
                       l1a=L1a_data_for_s2s3$l1a,verbs=list("fi"=GetPublishVerbs("fi","L1a"),"ru"=GetPublishVerbs("ru","L1a"))),langlist)

    # merkkaa julkaisuverbien läsnäolo kaikkiin g.df-listan datoihin
    for(lang in langlist){
        L1a_data_for_s2s3$l1a[[lang]]$hasPverb <- "no"
        L1a_data_for_s2s3$l1a[[lang]]$hasPverb[L1a_data_for_s2s3$l1a[[lang]]$headverb %in% L1a_data_for_s2s3$publishverbs[[lang]]$verbi] <- "yes"
        for (gname in names(g.df)){
            for(loc in names(g.df[[gname]])){
                g.df[[gname]][[loc]][[lang]]$hasPverb <- "no"
                g.df[[gname]][[loc]][[lang]]$hasPverb[g.df[[gname]][[loc]][[lang]]$headverb %in% L1a_data_for_s2s3$publishverbs[[lang]]$verbi] <- "yes"
            }
        }
    }

    L1a_data_for_s2s3$all.indicators <- list("ru"=subset(L1a_data_for_s2s3$l1a$ru, subjtype=="long"&hasPverb=="yes"&corpustype=="press"),
                          "fi"=subset(L1a_data_for_s2s3$l1a$fi, subjtype=="long"&hasPverb=="yes"&corpustype=="press"))

    L1a_data_for_s2s3$all.indicators$props <- lapply(L1a_data_for_s2s3$all.indicators,function(x)return(100*prop.table(table(x$location3))))
    L1a_data_for_s2s3$all.indicators$plot  <- ggplot(melt(matrix(c(L1a_data_for_s2s3$all.indicators$props$fi,L1a_data_for_s2s3$all.indicators$props$ru),nrow=2, byrow=T,dimnames=list(c("fi","ru"),c("S1","S2/S3","S4"))),varnames=c("lang","location")),
                            aes(x=location,y=value)) +  geom_bar(stat="identity", width=.3) + facet_grid(lang ~.)

    for(lang in langlist){
        L1a_data_for_s2s3$l1a[[lang]]$corpustype <- as.factor(L1a_data_for_s2s3$l1a[[lang]]$corpustype)
        L1a_data_for_s2s3$l1a[[lang]]$subtype <- as.factor(L1a_data_for_s2s3$l1a[[lang]]$subjtype)
        L1a_data_for_s2s3$l1a[[lang]]$hasPverb <- as.factor(L1a_data_for_s2s3$l1a[[lang]]$hasPverb)
    }
    L1a_data_for_s2s3$dataframe <- rbind(L1a_data_for_s2s3$l1a$fi,L1a_data_for_s2s3$l1a$ru)

    L1a_data_for_s2s3$l1a.hasPverb.proptabs <- setNames(lapply(langlist,function(lang,l1a) return(100*prop.table(xtabs(~location3, data=l1a[[lang]],subset=hasPverb=="yes"))), l1a=L1a_data_for_s2s3$l1a),langlist)
    save(L1a_data_for_s2s3, file="~/workprojects/phdR2/data/L1a_data_for_s2s3.rda")
}


#' Laske kehyksiseen resultatiiviseen funktioon liittyviä tilastoja erityisesti siitä, missä lauseissa on mukana numeraali
#' @export

GetKehRes <- function(){
        keh.res <- setNames(lapply(langlist, function(thislang){
                             numpat <- ifelse(thislang=="fi","\\[NUM>>","\\[M>>")
                             all.cases <- subset(d, funct=="res.keh"&lang==thislang)
                             numeric.cases <- all.cases[(which(grepl(numpat,all.cases$feats))),]
                             non.numeric.cases <- all.cases[(which(!grepl(numpat,all.cases$feats))),]
                             return(list("all"=all.cases,"number"=numeric.cases,"nonumber"=non.numeric.cases))
                             }),langlist)
        keh.res$all <- rbind(keh.res$fi$all,keh.res$ru$all)
}


