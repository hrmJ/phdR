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
