
#' Tulostaa listan tiettyyn semanttiseen funktioon kuuluvista aineistoryhmistä
#'
#' @param group ryhmän nimi
#' @param returnvector halutaanko, että funktio palauttaa vektorin ryhmistä
#'
#' @export

listmembers <- function(group, returnvector=F){
    durgroups <- sort(unique(d$group[d$funct == group]))
    if(returnvector){
        return(durgroups)
    }
    if(length(durgroups)>2){
        d1 <- paste(paste(durgroups[1:(length(durgroups)-2)], collapse=", "),", ", sep="")
        dgrouplist <- paste(d1,paste(durgroups[length(durgroups)-1], durgroups[length(durgroups)], sep = " ja "),sep="")
    }
    else if(length(durgroups)==2){
        dgrouplist <- paste(durgroups[1], "ja", durgroups[2])
    }
    else{
        dgrouplist <- durgroups[1]
    }
    dgrouplist
}


#' Tulostaa listan tiettyyn referentiaalisuuden tyyppiin kuuluvista
#' aineistoryhmistä
#'
#' @param group ryhmän nimi
#'
#' @export

listrefmembers <- function(group){
    durgroups <- sort(unique(d$group[d$ref == group]))
    if(length(durgroups)>2){
        d1 <- paste(paste(durgroups[1:(length(durgroups)-2)], collapse=", "),", ", sep="")
        dgrouplist <- paste(d1,paste(durgroups[length(durgroups)-1], durgroups[length(durgroups)], sep = " ja "),sep="")
    }
    else if(length(durgroups)==2){
        dgrouplist <- paste(durgroups[1], "ja", durgroups[2])
    }
    else{
        dgrouplist <- durgroups[1]
    }
    dgrouplist
}
