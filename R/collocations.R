
#' Hae yksinkertainen kollokaattilista
#' 
#' @param sourcedata lista (vektori) lauseista/konteksteista, joista kollokaatteja etsitään
#' @param targetpattern se merkkijono(REGEX), jolle kollokaatteja etsitään
#' @param maxcontext miten pitkälle kollokaatteja kartotetaan
#'
#' @importFrom stringi stri_trim_both stri_split_boundaries
#' @export

SimpleCollocations <- function(sourcedata, targetpattern, maxcontext=1){
    colloclist <- list("left"=list(),"right"=list())
    targetrep <- 'tttttargetttttt'
    for(idx in c(1:length(sourcedata))){
        #Huom! poista mahdollisesti tekstissä olevat doc-tägit
        s<-gsub("<doc>","",sourcedata[idx], ignore.case=T)
        #Varsinainen muokkaus:
        s<-gsub(targetpattern,targetrep,s, ignore.case=T)
        s<-gsub('[[:punct:]]+',' ',s)
        words   <- stri_trim_both(stri_split_boundaries(s, type="word")[[1]], pattern = "\\P{Wspace}")
        words <- tolower(unname(unlist(sapply(words,function(x)if(x!="")return(x)))))
        target.idx <- which(words==targetrep)
        if(length(target.idx>1)) target.idx <- target.idx[1]

        for(subidx in c(1:maxcontext)){
            if(length(target.idx)>0){
                if(length(colloclist[["right"]])<subidx){
                    colloclist[["right"]][[subidx]] <- c("")
                }

                if(target.idx+subidx<length(words)){
                    #Lisää oikeanpuoleisten kollokaattien listaan sana, joka on subidx askelta oikealla
                    colloclist[["right"]][[subidx]] <- c(colloclist[["right"]][[subidx]],words[target.idx+subidx]) 
                } 
                else{
                    colloclist[["right"]][[subidx]] <- c(colloclist[["right"]][[subidx]],"---") 
                }


                if(length(colloclist[["left"]])<subidx){
                    colloclist[["left"]][[subidx]] <- c("")
                }
                if(target.idx-subidx>0){
                    colloclist[["left"]][[subidx]] <- c(colloclist[["left"]][[subidx]],words[target.idx-subidx]) 
                } 
                else{
                    colloclist[["left"]][[subidx]] <- c(colloclist[["left"]][[subidx]],"---") 
                }
            }
            else{
                colloclist[["left"]][[subidx]] <- c(colloclist[["left"]][[subidx]],"ERROR") 
                colloclist[["right"]][[subidx]] <- c(colloclist[["right"]][[subidx]],"ERROR") 
            }
        }
    }
    return(colloclist)
}

#' Luo SimpleCollocations-funktion avulla taulukon eri kollokaattien yleisyydestä
#' Lisäksi tallentaa yksittäisten kollokaattien tiedot ja palauttaa kaiken informaation yhtenä isona listana
#'
#' @param mydf data.frame, jonka pohjalta luodaan
#' @param colpat Regex siitä, minkä kollokaatteja etsitään
#' @export

CollocateTable <- function(mydf,colpat,colrange=3){
    cols <- SimpleCollocations(mydf$sent,colpat,colrange)
    returns = list()
    for (r in c(1:colrange)){
        returns[[r]] <- list(left=list(),right=list())
        for(direction in c("left","right")){
            tabs <- table(cols[[direction]][[r]])
            tabs.df <- as.data.frame(tabs[order(tabs,decreasing=T)],)
            colnames(tabs.df) <- c("sana","frekvessi")
            returns[[r]][[direction]]<- list(tab=tabs,df=tabs.df,cols=cols[[direction]][[r]][2:(nrow(mydf)+1)])
        }
    }
    return(returns)
}
