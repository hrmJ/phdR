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
        kable(.,caption=cap, longtable=T, booktabs=T)
}
