
#' Tulostaa kuvion, jossa vertailtu suomen ja venäjän prosenttiosuuksia
#' 
#' @param  subsetted dataframe, josta kuvio tehdään
#' @param  loc sijainti, jota tutkitaan
#' @param  dif.treshold kuinka pienet erot jätetään pois kuviosta
#' @param  langs vertailtavat kielet
#' @param  percentual järjestetäänkö suhteellisen eron mukaan vai absoluuttisen
#' @param  means piirretäänkö keskiarvoviivoja ja millä arvoilla
#' 
#' @importFrom gdata reorder.factor
#' @importFrom reshape melt
#' @importFrom dplyr arrange %>%
#' @importFrom ggplot2 ggplot geom_bar aes position_dodge
#' @export
#'

PrepareOrderedDifPlot <- function(subsetted, loc, dif.treshold, langs, percentual=T, means=vector()){

    fitb <- melt(prop.table(xtabs(~location3 + group,data=subsetted, subset=lang=="fi"),2)*100)
    rutb <- melt(prop.table(xtabs(~location3 + group,data=subsetted, subset=lang=="ru"),2)*100)
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
    neworder <- factor(names(s1adifs.top), levels=names(s1adifs.top))
    #http://stackoverflow.com/questions/11977102/order-data-frame-rows-according-to-a-target-vector-that-specifies-the-desired-or
    plotted.s1a$group <- reorder.factor(plotted.s1a$group, new.order=neworder)
    plotted.s1a <- na.omit(plotted.s1a %>% arrange(group))
    pl  <- ggplot(plotted.s1a,aes(x=group, y=value, fill=lang)) +
        geom_bar(stat="identity", position=position_dodge(width=.5),width=.4) +
        theme_bw() + scale_fill_grey(start = 0.3, end = .7) +  coord_flip() + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank())

    if(hasArg(means)){
        pl  <- pl +
            geom_hline(aes(yintercept=means$ru), color="grey70",linetype="dotted") +
            geom_hline(aes(yintercept=means$fi), color="grey30", linetype="dotted")  
    }

    return(pl)


}

