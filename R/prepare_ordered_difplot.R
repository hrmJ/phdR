
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
#' Muokkaa numerot näkymään kivemmin
#' 
#' @export

fn <- function(n,numbers=2){
    numstring <- formatC(round(n,numbers),numbers,format="f")
    return(gsub('\\.',',',numstring))
}


#' Tulostaa taulukon, jossa kielikohtaisesti sijainnit (n + pros)
#' 
#' @param sourcetibble varsinainen data
#' @param col1 ensimmäisen sarakkeen nimi
#' @param col2 toisen sarakkeen nimi
#' @param cap taulukon otsikko
#' 
#' @importFrom tidyr spread_
#' @importFrom dplyr %>% mutate group_by_ count_ left_join select
#' @importFrom kableExtra kable
#' @export

PrintLocationTableWithCounts <- function(sourcetibble, col1, col2, cap){
    props <- round(prop.table(
                              xtabs(~sourcetibble[[col1]] + sourcetibble[[col2]]),
                              1) * 100,2)  %>%  as_tibble
    colnames(props) <- c(col1,col2,"n")
    sourcetibble %>% 
        group_by_(col1,col2)  %>% count_(col1)  %>% 
        left_join(.,props,by=c(col1,col2)) %>% 
        mutate(n=paste0(paste0(format(n.y,decimal.mark=",")," %"),
                       " (", format(n.x,big.mark=" "), ")")) %>% 
        select_(col1,col2,"n")  %>% 
        spread_(key=col1,value="n")  %>% 
        t  %>% 
        `colnames<-`(.[1,]) %>% 
        .[-1,]  %>% 
        kable(.,caption=cap, longtable=T, booktabs=T) %>% 
        kable_styling (full_width = T)
}



#' Tulostaa side-by-side-tyyppisen taulukon
#' 
#' Taulukossa yhtä monta saraketta suomessa ja venäjässä
#' 
#' @param tabs lista vierekkäin tulevista taulukoista
#' @param cnames sarakkeiden nimet
#' @param gheadings nimetty vektori, joka kertoo myös yhden taulukon sarakkeiden määrän
#' @param cap Taulukon otsikko
#' 
#' @importFrom dplyr mutate  %>% select right_join everything
#' @importFrom kableExtra kable add_header_above column_spec
#' 
#' @export
PrintCompTable <- function(tabs, cnames, gheadings, cap){

    tabs <- lapply(tabs,function(x){
       x %>% 
           mutate(rank=rownames(.)) %>% 
           select(rank,everything())  
    })

    tabs[[1]]  %>% 
        right_join(tabs[[2]], by="rank")  %>% 
        select(-rank) %>% 
        setNames(rep(cnames,2)) %>% 
        kable(booktabs=T,longtable=T, caption=cap) %>% 
        add_header_above(gheadings) %>% 
        column_spec(3, border_right=T) %>% 
        kable_styling (full_width = T)

}


#' Oikopolku ryhmien kuvausten tulostamiseen taulukoina
#'
#'
#' @importFrom kableExtra kable kable_styling column_spec
#' @importFrom dplyr  %>% rename
#'
#' @export

PrintGroupTable <- function(g, fi, ru){
    gtext1 <- ifelse(length(g)==1,"Aineistoryhmään ","Aineistoryhmiin ")
    if(length(g)>2){
        gtext2 <- paste0(paste(g[1:length(g)-1],collapse=", ")," ja ", g[length(g)])
    }
    else if(length(g)>1){
        gtext2 <- paste0(g[1]," ja ", g[2])
    }
    else{
        gtext2 <- g[1]
    }

    cap <- paste0(gtext1, gtext2, " kuuluvat ilmaukset suomessa ja venäjässä.")

    cbind(g, fi, ru) %>% 
        as_tibble %>% 
        setNames(c("Koodi", "suomi", "venäjä")) %>% 
        kable(booktabs=T,longtable=T, caption=cap) %>% 
        kable_styling(full_width=T)  %>% 
        column_spec(1, width="1.2cm") 
}


#' Oikopolku satunnaisotantataulukoihin
#'
#'
#' @importFrom kableExtra kable kable_styling column_spec row_spec
#' @importFrom dplyr  %>% select mutate case_when desc arrange
#' @importFrom tidyr  spread
#'
#' @param input syötteenä oleva tibble
#' @param cap taulukon otsikko
#'
#' @export

PrintSampleTable <- function(input, cap){
    if("cxg" %in% colnames(input)){
        input <- input %>% rename(cx=cxg)
    }
    tab <- input %>% 
        count(lang,cx) %>% 
        spread(key="lang",value="n",fill=0) 
    tab$n <- apply(tab,1,function(x){sum(as.integer(x[2:3]))})
    tab %>% mutate(cx=case_when(
                             cx %in% c("aff","affekt") ~ "affektiivinen konstruktio",
                             cx == "alatop" ~ "alatopiikkikonstruktio",
                             cx == "anaf" ~ "anaforinen konstruktio",
                             cx == "kataf" ~ "kataforinen konstruktio",
                             cx == "muu" ~ "ei luokiteltavissa",
                             cx == "kontr" ~ "kontrastiivinen konstruktio",
                             cx == "ei-temp" ~ "ei--temporaalinen вдруг-konstruktio",
                             cx == "ei-temps" ~ "ei--temporaalinen silloin-konstruktio",
                             cx == "ei-tempj" ~ "ei--temporaalinen jälkeen-konstruktio",
                             cx == "määrä" ~ "määrää painottava konstruktio",
                             cx == "adv" ~ "adverbinen konstruktio",
                             cx == "sekv" ~ "anaforinen konstruktio",
                             cx == "tiiv.anaf" ~ "Tiivistetty anaforinen konstruktio",
                             cx == "topik" ~ "topikaalinen konstruktio",
                             cx == "johdanto" ~ "johdantokonstruktio",
                             cx == "lok.johd" ~ "lokaali johdantokonstruktio",
                             cx == "glob.johd" ~ "globaali johdantokonstruktio",
                             cx == "top" ~ "topikaalinen konstruktio",
                             cx == "fok" ~ "fokaalinen konstruktio",
                             cx == "rtu" ~ "johdantokonstruktio",
                             cx == "äkkiä" ~ "äkkiä-konstruktio",
                             TRUE ~ cx
                             ))  %>% 
        arrange(desc(n)) %>% 
        add_row(cx="Yht.",fi=sum(.$fi),ru=sum(.$ru),n=sum(fi,ru)) %>% 
        setNames(c("Konstruktio","n / suomi","n / venäjä", "Yht.")) %>% 
        kable(booktabs=T,longtable=T,caption=cap) %>% 
        kable_styling (full_width = T) %>%
        row_spec(nrow(tab), hline_after = T) %>% 
        column_spec(1, width="9cm")
}


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
        arrange(group)  

    pl  <- ggplot(plotted.s1a,aes(x=group, y=value, fill=lang)) +
        geom_bar(stat="identity", position=position_dodge(width=.5),width=.4) +
        geom_text(aes(label=paste0(ns1, " / ", nall, " kpl"),vjust=-0.2),size=2) + 
        theme_bw() + scale_fill_grey(start = 0.3, end = .7) +
        coord_cartesian(ylim = c(0,1)) +
        scale_y_continuous(labels = percent) +
        #coord_flip() + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank())

    if(hasArg(means)){
        pl  <- pl +
            geom_hline(aes(yintercept=means$ru/100), color="grey70",linetype="dotted") +
            geom_hline(aes(yintercept=means$fi/100), color="grey30", linetype="dotted")  
    }

    return(pl)


}

