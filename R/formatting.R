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
    mutated <- sourcetibble

    if(col1=="lang"){
        mutated <- sourcetibble  %>% mutate(lang=case_when(lang=="ru"~"venäjä",T~"suomi"))
    }

    props <- round(prop.table(
                              xtabs(~mutated[[col1]] + mutated[[col2]]),
                              1) * 100,2)  %>%  as_tibble
    colnames(props) <- c(col1,col2,"n")


    mutated %>% 
        group_by_(col1,col2)  %>% count_(col1)  %>% 
        left_join(.,props,by=c(col1,col2)) %>% 
        mutate(n=paste0(paste0(format(n.y,decimal.mark=",")," %"),
                       " (", format(n.x,big.mark=" "), ")")) %>% 
        select_(col1,col2,"n")  %>% 
        spread_(key=col1,value="n")  %>% 
        t  %>% 
        `colnames<-`(.[1,]) %>% 
        .[-1,]  %>% 
        kable(.,caption=cap,  booktabs=T) %>% 
        kable_styling (latex_options=c("HOLD_position"), full_width = T) 

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
        kable(booktabs=T,  caption=cap) %>% 
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
        kable(booktabs=T,  caption=cap) %>% 
        kable_styling(latex_options=c("HOLD_position"), full_width=T)  %>% 
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
#' @export

PrintSampleTable <- function(input, cap, just_data=F){
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
                             cx == "durpaikka" ~ "duratiivi + paikka -konstruktio",
                             cx == "tiivhist" ~ "tiivistetty historia -konstruktio",
                             cx == "kontr" ~ "kontrastiivinen konstruktio",
                             cx == "ei-temp" ~ "ei--temporaalinen вдруг-konstruktio",
                             cx == "ei-temps" ~ "ei--temporaalinen silloin-konstruktio",
                             cx == "ei-tempj" ~ "ei--temporaalinen jälkeen-konstruktio",
                             cx == "määrä" ~ "määrää painottava konstruktio",
                             cx == "adv" ~ "adverbinen konstruktio",
                             cx == "adverbi" ~ "adverbinen konstruktio",
                             cx == "mainostava" ~ "ilmoituksen aloittava konstruktio",
                             cx == "sekv" ~ "anaforinen konstruktio",
                             cx == "tiiv.anaf" ~ "Tiivistetty anaforinen konstruktio",
                             cx == "top" ~   "topikaalinen konstruktio",
                             cx == "topik" ~ "topikaalinen konstruktio",
                             cx == "ten" ~   "topikaalinen konstruktio",
                             cx == "johdanto" ~ "johdantokonstruktio",
                             cx == "joskusjoskus" ~ "joskus--joskus-konstruktio",
                             cx == "glob.johd" ~ "globaali johdantokonstruktio",
                             cx == "glob.johd." ~ "globaali johdantokonstruktio",
                             cx == "lok.johd" ~ "lokaali johdantokonstruktio",
                             cx == "lok.johd." ~ "lokaali johdantokonstruktio",
                             cx == "fok" ~ "fokaalinen konstruktio",
                             cx == "rtu" ~ "johdantokonstruktio",
                             cx == "äkkiä" ~ "äkkiä-konstruktio",
                             TRUE ~ cx
                             ))   -> temp
    
        if(input %>% filter(lang=="fi")  %>% nrow == 0){
            temp <- temp %>% 
                arrange(desc(ru))  %>% 
                select(-n) %>% 
                add_row(cx="Yht.",ru=sum(.$ru))  %>% 
                setNames(c("Konstruktio","n / venäjä")) 
        }
        else if(input %>% filter(lang=="ru")  %>% nrow == 0){
            temp <- temp %>% 
                arrange(desc(fi))  %>% 
                select(-n) %>% 
                add_row(cx="Yht.",fi=sum(.$fi))  %>% 
                setNames(c("Konstruktio","n / suomi")) 
        }
        else{
            temp <- temp %>% 
                arrange(desc(n))  %>% 
                add_row(cx="Yht.",fi=sum(.$fi),ru=sum(.$ru),n=sum(fi,ru))  %>% 
                setNames(c("Konstruktio","n / suomi","n / venäjä", "Yht.")) 
        }
        if(just_data == T){
            return (temp)
        }
        

        temp %>% 
            kable(booktabs=T, caption=cap) %>% 
            kable_styling (latex_options=c("HOLD_position"), full_width = T) %>%
            row_spec(nrow(tab), hline_after = T) %>% 
            column_spec(1, width="9cm")  %>% 
            gsub("\\\\addlinespace","", .)

}

