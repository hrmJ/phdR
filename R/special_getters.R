#' Tähän tiedostoon on listattu funktioita, joiden tehtävä on, mahdollisesti
#' aika kertakäyttöisesti, hakea dataa tietyssä muodossa tai tietyllä tavalla
#' filtteröitynä isosta d-dataframesta

#' Hae adverbien osuus tietyssä sijainnissa
#' @param x ryhmä, jonka osuuksia tarkastellaan
#' @param lang kieli
#' @param total dataframe, josta haetaan
#' @param location sijainti, josta haetaan
#' @export
GetAdvShare  <- function(x, lang, total, location){
    share  <-  nrow(total[total$group==x&total$lang==lang&total$location3=="S2/S3",])/nrow(total[total$group==x&total$lang==lang,])
    return(share)
} 



#' Hae kaikki yhden kielen "julkaisuverbeiksi" luokitellut verbit pakettiin sisällytestä datatiedostosta
#' @param lang kieli (fi/ru)
#' @param groupname aineistoryhmä, jonka perusteella verbejä etsitään
#' @importFrom readr read_lines
#' @export
GetPublishVerbs <- function(lang, groupname){
    f <- read_lines(system.file("extdata/verbs",paste0(lang,"_",tolower(groupname),".txt"),package="phdR2"))
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




