
#' Tulostaa kuvion, jossa vertailtu suomen ja venäjän prosenttiosuuksia
#' 
#' @param  subsetted dataframe, josta kuvio tehdään
#' @param  loc sijainti, jota tutkitaan
#' @param  dif.treshold kuinka pienet erot jätetään pois kuviosta
#' @param  langs vertailtavat kielet
#' @param  percentual järjestetäänkö suhteellisen eron mukaan vai absoluuttisen
#' @param  means piirretäänkö keskiarvoviivoja ja millä arvoilla
#' 
#' @importFrom reshape melt
#' @importFrom dplyr arrange %>%
#' @importFrom ggplot2 ggplot geom_bar aes position_dodge
#' @importFrom scales percent
#' @export
#'

PrepareOrderedDifPlot <- function(subsetted, loc, dif.treshold, langs, percentual=T, means=vector()){

    fitb <- melt(prop.table(xtabs(~location3 + group,data=subsetted, subset=lang=="fi"),2))
    rutb <- melt(prop.table(xtabs(~location3 + group,data=subsetted, subset=lang=="ru"),2))
    fitb$lang <- "fi"
    rutb$lang <- "ru"
    s1adv <- rbind(fitb,rutb)
    s1adv <- s1adv[s1adv$location3==loc,c(2:4)]

    # Silmämääräistä vertailua varten

    #Numeeriset erot eri adverbisten ryhmien s1-osuuksissa:
    if(percentual==T){
       s1adifs <- s1adv[s1adv$lang==langs[2],"value"]  / s1adv[s1adv$lang==langs[1],"value"]
        names(s1adifs) <- s1adv$group[s1adv$lang==langs[1]]
        s1adifs <- s1adifs[order(s1adifs,decreasing=F)]
    }
    else{
        s1adifs <- s1adv[s1adv$lang==langs[1],"value"]  - s1adv[s1adv$lang==langs[2],"value"]
        names(s1adifs) <- s1adv$group[s1adv$lang==langs[1]]
        s1adifs <- s1adifs[order(s1adifs,decreasing=T)]
    }


    if(is.null(dif.treshold)){
        s1adifs.top <- s1adifs
    }
    else{
        s1adifs.top <- s1adifs[s1adifs>dif.treshold]
    }
    plotted.s1a <- s1adv[s1adv$group %in% names(s1adifs.top),]

    #Järjestä uudelleen eron mukaan
    target <- as.character(names(s1adifs.top))
    plotted.s1a <- plotted.s1a %>%  
        right_join(tibble(group=target), by="group") %>% 
        na.omit %>% 
        mutate(group=factor(group,levels=unique(group))) %>% 
        arrange(group)

    nvals <- subsetted %>% 
        count(group,location3, lang) %>%
        filter(location3 == loc) %>% 
        select(-location3) %>% 
        right_join(tibble(group=target), by="group") %>% 
        mutate(group=factor(group,levels=unique(group))) %>% 
        arrange(group) %>% 
        right_join(subsetted %>% count(group,lang),by=c("group","lang")) %>% 
        rename(ns1=n.x, nall=n.y)

    plotted.s1a <- plotted.s1a %>% 
        left_join(nvals,by=c("group","lang"))  %>% 
        mutate(group=factor(group,levels=unique(group))) %>% 
        arrange(group)   %>% 
        mutate(kieli=case_when(lang=="ru"~"venäjä",T~"suomi"))

    pl  <- ggplot(plotted.s1a,aes(x=group, y=value, fill=kieli)) +
        geom_bar(stat="identity", position=position_dodge(width=.5),width=.4) +
        geom_text(aes(label=paste0(ns1, " / ", nall)),size=1.6,angle=90, hjust=-0.1, position=position_dodge(width=.5)) + 
        theme_bw() + scale_fill_grey(start = 0.3, end = .7) +
        coord_cartesian(ylim = c(0,1)) +
        scale_y_continuous(labels = percent) +
        #coord_flip() + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.title=element_text(size=7),
              axis.text=element_text(size=6)
              ) 



    if(hasArg(means)){
        pl  <- pl +
            geom_hline(aes(yintercept=means$ru/100), color="grey70",linetype="dotted") +
            geom_hline(aes(yintercept=means$fi/100), color="grey30", linetype="dotted")  
    }

    return(pl)


}

