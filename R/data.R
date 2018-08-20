
#' Tutkimuksen tilastollisen mallin ym. kannalta oleellinen numeerinen data 
#'
#' Tästä julkisesti saatavilla olevasta datasta tarkoituksella jätetty pois mahdollisesti sensitiivinen, varsinaisia tekstejä sisältävä korpusdata
#'
#' @format tibble seuraavalla rakenteella
#' \describe{
#'   \item{lang}{lauseen kieli}
#'   \item{group}{esimerkin aineistoryhmä}
#'   \item{funct}{semanttinen funktio}
#'   \item{morph}{morfologinen rakenne}
#'   \item{location3}{kolmijakoinen sijaintimuuttuja (S1, S2/S3, S4)}
#'   \item{location}{nelijakoinen sijaintimuuttuja (S1,S2,S3,S4)}
#'   \item{isnumeric}{onko lauseessa läsnä numeraali - tarkemmin ks. funktio DoesThisRowHaveNumeral}
#'   \item{pos}{positionaalisuus}
#'   \item{red}{referentiaalisuus}
#'   \item{corpustype}{lähdekorpuksen tyyppi}
#'   \item{sentid}{lauseen alkuperäinen id tietokannassa}
#'   ...
#' }
#'
"d"



#' Venäjän ja suomen SOV-lauseita koskeva numeerinen data.
#'
#'
#' @format tibble seuraavalla rakenteella
#' \describe{
#'   \item{lang}{lauseen kieli}
#'   \item{group}{esimerkin aineistoryhmä}
#'   \item{funct}{semanttinen funktio}
#'   \item{morph}{morfologinen rakenne}
#'   \item{location3}{kolmijakoinen sijaintimuuttuja (S1, S2/S3, S4)}
#'   \item{location}{nelijakoinen sijaintimuuttuja (S1,S2,S3,S4)}
#'   \item{isnumeric}{onko lauseessa läsnä numeraali - tarkemmin ks. funktio DoesThisRowHaveNumeral}
#'   \item{pos}{positionaalisuus}
#'   \item{red}{referentiaalisuus}
#'   \item{corpustype}{lähdekorpuksen tyyppi}
#'   \item{sentid}{lauseen alkuperäinen id tietokannassa}
#'   ...
#' }
#'
"sov"


#' Tutkimuksen varsinaiset kontekstit
#'
#' (data ei julkista)
#'
#' @format tibble
#'
#' "sents"


#' Esimerkkilauseet, joilla testataan parserin tarkkuutta
#'
#' Aineistoksi filtteröidyistä lauseista valittiin satunnaisotannalla 
#'
#' @format Data frame, jonka rakenne on seuraava
#' \describe{
#'   \item{sent}{esimerkkivirke}
#'   \item{group}{esimerkin aineistoryhmä}
#'   \item{lang}{virkkeen kieli}
#'   \item{correct}{Onko parseri analysoinut oikein (T/F)}
#'   ...
#' }
"parser_accuracy_test"



#' Kunkin aineistoryhmän ominaisuudet taksonomioiden 1--3 valossa
#'
#'
#' @format tibble
#'
"groupmeta"


