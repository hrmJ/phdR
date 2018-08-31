
#' Returns the ratio of sentences with a numeral compared to all sentences in a specific data group
#' 
#' @export

GiveNumericals <- function(gname,mydf,lang, loc="S1"){
    ss <- mydf[mydf$group==gname&mydf$location3==loc&mydf$lang==lang,]
    numerical <- ss[ss$isnumeric=="has numeral",]
    return (nrow(numerical)/nrow(ss)*100)
}



#' Määrittele jostakin tutkimusdatataulukon rivistä, onko siinä (ajanilmauksen lisäksi) jokin numeraali
#'
#' Numeraalin määrittelylle asetetaan seuraavat kriteerit: 1.kardinaaliluku 2. ei tyypillinen vuosiluku 3. ei ilmaus n vuotta
#' LISÄKSI oletetaan, että seuraavan sanan on oltava suomessa partitiivissa, venäjässä genetiivissä.
#'
#' @importFrom readr write_lines
#' @export

DoesThisRowHaveNumeral <- function(thisrow){

     lang  <- thisrow["lang"]
     feats  <- thisrow["posfeatlist"][[1]]
     sent  <- thisrow["sent"]
     group  <- thisrow["group"]
     accepted  <- "has numeral"
     not_accepted  <- "no numeral"
     punct <- c(".",",","!","?",":",";","—")
     if(is.na(thisrow[["funct"]])){
         show("WARNING! no funct.. assigned")
     }
     #Hylkää automaattisesti presuppositionaaliset:
     if ( thisrow[["funct"]] == "presup" | thisrow[["group"]] %in% c("L5c")){ 
         return ( not_accepted )
     }
     #1. siistitään: 
     sent  <- gsub("(<\\/ doc>|<doc>|<doc >|<ref>|<li>|<ul>|<a>|<p>|<div>|<ol>|<ul>|<span>|<p>|<h1>|<h2>|<Joulutin>)","",sent)
     #Jaotellaan lause saneisiin(mukaanlukien välimerkit)
     #Suomen päivämäärät
     sent  <- gsub("\\d{1,2}\\.\\d{1,2}\\.\\d{4}","PVM",sent,ignore.case=T)
     sent  <- gsub("0\\d{1,2}\\.0\\d{1,2}\\.\\d{4}","PVM",sent,ignore.case=T)
     #Desimaalit!
     sent  <- gsub("(\\d+)(,|\\.)(\\d+)","\\1point\\3",sent,ignore.case=T)
     #Tarkat kellonajat
     sent  <- gsub("\\d{2}:\\d{2}","KLO",sent,ignore.case=T)
     #Urheilutulos
     sent  <- gsub("\\d{1,2}(-|–){1,2}\\d{1,2}","SPORT",sent,ignore.case=T)
     sent  <- gsub("\\d{4}(-|–){1,2}\\d{4}","RANGE",sent,ignore.case=T)
     #ISO sertifikaattinumero
     sent  <- gsub("ISO \\d+","ISOSERT ISOSERT",sent,ignore.case=T)
     #Venäjän nimilyhenteet, joissa ei väliä + Muuta
     if(lang=="ru"){
         sent  <- gsub("([А-Я,A-Ö]\\.[а-я,a-ö])+","IMJA_FAMILIJA",sent, ignore.case=T)
         sent  <- gsub("([А-Я,A-Ö]\\.[а-я,a-ö])+","IMJA_FAMILIJA",sent, ignore.case=T)
         sent  <- gsub("[А-Я]\\.[А-Я][а-я]+","IMJA_FAMILIJA",sent)
         sent  <- gsub("[A-Ö]\\.[A-Ö][a-ö]+","IMJA_FAMILIJA",sent)
         sent  <- gsub("[а-я]+,[а-я]+","PARSER_FAILED",sent)
     }
     #show(sent)
     #Huolehditaan, ettei suomen kaksoispisteellä lisätty taivutuspääte sekoita:
     sent  <- gsub(":([a-ö])","\\1",sent,ignore.case=T)
     #Lisätään välit välimerkkien ympärille, jotta nekin laskettaisiin
     sent  <- gsub("([\\(\\),\\.\\?\\!\\:;'\"])"," \\1 ",sent)
     tokens <- strsplit(sent, "\\s+")[[1]]
     #jaotellaan kunkin saneen piirteet omaan vektoriinsa
     #Siistitään myös doc:sta saastuneet piirresarjat:
     feats <- gsub("\\[NOUN>>Case=Nom\\|Number=Sing\\]\\[SYM>>_\\]","",feats,ignore.case=T)
     #Poistetaan joitakin suomen parserin lisämerkintöjä:
     feats <- gsub("\\[psor\\]","",feats,ignore.case=T)
     feats_vector <- strsplit(feats, "\\]")[[1]]

     #varmistetaan, ettei mukaan ole päässyt tyhjiä alkioita
     tokens <- tokens[which(tokens != "")]
     feats_vector <- feats_vector[which(feats_vector != "")]
     #return(list(t=tokens,f=feats_vector))

     #paikannetaan etsittävä ajanilmaus
     advloc <- which(grepl("<[a-öа-яё]+>",tokens,ignore.case=T))
     starttoken<-advloc
     endtoken<-advloc

     #show(tokens)
     #show(feats_vector)

     #Määrittele aj.ilm. sisältävän lauseen alku
     while(starttoken>1){
         starttoken <- starttoken - 1
         thistoken <- tokens[starttoken]
         if(thistoken %in% punct){
             starttoken <- starttoken + 1
             break
         }
     }

     #Määrittele aj.ilm. sisältävän lauseen loppu
     while(endtoken<length(tokens)){
         endtoken <- endtoken + 1
         thistoken <- tokens[endtoken]
         if(thistoken %in% punct){
             endtoken <- endtoken - 1
             break
         }
     }

     #määritellään uudestaan, nyt pelkälle lauseelle:
     tokens <- tokens[c(starttoken:endtoken)]
     feats_vector <- feats_vector[c(starttoken:endtoken)]
     advloc <- which(grepl("<",tokens))
     margins <- DetermineAdvPhraseLength(thisrow, feats_vector)

     #Poistetaan sulkeet venäjän sanelistasta, koska niitä ei ole erikseen annotoitu
     if(lang=="ru"){
         newtokens  <- c()
         newfeats  <- c()
         for(i in c(1:length(tokens))){
             token  <- tokens[i]
             feat  <- feats_vector[i]
             if(!token %in% c("(",")")){
                 newtokens <- c(newtokens, token)
                 newfeats <- c(newfeats, feat)
             }
         }
         tokens <- newtokens
         feats_vector <- newfeats
     }



     #Määritellään parserin merkintä kardinaaliluvulle
     card  <- ifelse(lang=="fi","NumType=Card","\\[M>>Mc[^]]")
     part  <- ifelse(lang=="fi","Case=Par",">((A|P)[^]]{4}g|N[^]]{3}g)")

     #show(tokens)
     #show(feats_vector)

     #Käydään läpi jokainen piirrevektorin alkio
     phrasestart <- advloc - margins$substractedmargin
     phraseend <- advloc + margins$addedmargin
     advphrasetokens <- c(phrasestart:phraseend)
     vuotias <- c("vuotiaan","vuotiaisiin","vuotiaaseen","vuotiaasta","vuotiaana","vuotiaissa","vuotiaiden","vuotiaista","vuotiaisista","vuotiaita","vuotiasta","vuotias")
     vuosi_tms <- c("vuotta","viikkoa","недель","недели","vuoden","лет","года","год","месяца","месяцев","vuoteen","asti","kuukauden","kuukautta","tuntia","minuuttia","tunnin","minuutin","vrk","vuorokauden")
     smallnumbers <- c("yksi","kaksi","kolme","pari","neljä","один","два","три","черыре","одного","двух","трех","четырех","kuusi","viisi","пять","шесть","пяти","шести")
     for(i in c(1:length(feats_vector))){
         thistoken  <- tokens[[i]]
         if(!i %in% advphrasetokens){
             #Tarkistetaan vain sanat, jotka eivät kuulu itse adverbilausekkeeseen
             thisfeat  <- feats_vector[[i]]
             thistoken  <- tokens[[i]]
             #show(thistoken)
             #show(thisfeat)
             #show(gsub(">>[^]]+","",thisfeat))
             if(thistoken %in% c("ISOSERT","PVM","KLO","SPORT","RANGE") | grepl("(SPORT|RANGE)",thistoken)
                |grepl("luvulla",thistoken)
                |grepl("yhdeltä|kahdelta|kolmelta|neljältä|viideltä|kuudelta|seitsemältä|kahdeksalta|yhdeksältä",thistoken)
                ){
                 #suomen päivämäärät yms., jotka edellä kartoitettiin
                 write_lines(paste(thistoken,">>",sent), "/tmp/failed_beacause_CARD_but_PVM.txt",append=T)
                 next
             }
             if(grepl(card, thisfeat)){
                 #1. Jos tämä sana on kardinaalilukusana

                 if(i>1){
                     #jos tätä sanaa edeltää sana vuonna (tms.), skippaa tämä kierros
                     #Myös tietyistä prepositioista voidaan määrittää, etteivät ne ole
                     #se, mitä tässä haetaan
                     #Skipataanmyös, jos edellinen sana prepositio за (tällöin ei määrää vaan esim. hintaa)
                     previoustoken  <- tokens[[i-1]]
                     if(tolower(previoustoken) %in% c("vuonna","vuodesta","Август","к","klo","vuodelle","vuosille","kello") |
                        tolower(previoustoken) %in% c("tyypin")  |
                        tolower(previoustoken) %in% c("за","по","в")  
                        ){
                         # Loggaa hylkäystapaukset
                         write_lines(paste(thistoken,">>",sent), "/tmp/failed_beacause_CARD_but_VUONNA.txt",append=T)
                         next
                     }
                 }

                 #jos tähän sanaan kuuluu 'vuotias' tai 'летний', skippaa
                 #skippaa myös оба jne
                 #Myös kollektiivilukusanat skipataan
                 unaccepted_strings <- c("летний","летняя","летнее","летнего","летней","летнюю","летних")
                 if(grepl(paste0("(",paste0(unaccepted_strings,collapse="|"),")"),thistoken,ignore.case=T)|
                    tolower(thistoken) %in% c("много","оба","обе","обоих","обоим") |
                    grepl("(веро|верых)",thistoken)
                    ){
                     write_lines(paste(thistoken,">>",sent), "/tmp/failed_beacause_CARD_but_LETNIJ.txt",append=T)
                     next
                 }

                 if(!thistoken %in% as.character(c(1800:2050))){
                     #2b. Jos tämä sana ei ole tyypillinen vuosiluku
                     if(i<length(feats_vector)){
                         nexttoken  <- tokens[[i+1]]
                         nextfeat  <- feats_vector[[i+1]]
                         nexttoken2  <- ifelse(i<length(feats_vector)-1,tokens[[i+2]],FALSE)
                         if(!tolower(nexttoken) %in% vuosi_tms
                            & !tolower(nexttoken) %in% vuotias
                            & !tolower(nexttoken) %in% patterns$ru$months$partitive
                            & !tolower(nexttoken) %in% patterns$fi$months$partitive
                            & !tolower(nexttoken) %in% c("metrin","метров","neliön","квадратных","квадратов","metrillä","kilometriä","километров","kilometrin","metriä","mg","m","cm","мг","м","kiloa","kg","кг")
                            & !tolower(nexttoken) %in% c("luvun","годов","luvulta","lukuun","momentissa","momentin","momentti","момента","päivän")
                            & !tolower(nexttoken) %in% c("tappion","voiton","tappionsa","voittonsa","johdon","johtonsa","johdoksi","tappioksi","voitoksi","tasapelin","ничью")
                            & !tolower(nexttoken) %in% c("kohdan","pykälän","статье","статьи","параграфа","пункта","типа","op","kääpiötä","гномов")
                            & !tolower(nexttoken) %in% c("часов","часа","час","минут","секунд","viikoksi","<viikoksi>")
                            & !tolower(nexttoken) %in% c("astetta","градусов","градуса","века","веков","kertaa","раза","раз")
                            & !tolower(nexttoken) %in% c("утра","вечера","уверен")
                            & !tolower(nexttoken) %in% c("asukkaan")
                            & !grepl("уверен", nexttoken)
                            & !grepl("vuotis",nexttoken,ignore.case=T)
                            & !grepl("maksoi.*euroa",sent,ignore.case=T)
                            & !grepl("ksi>",nexttoken,ignore.case=T)
                            ){
                             #3. Jos kardinaalilukusana, ei tyypillinen vuosiluku eikä perässä vuosi-sanaa tms.
                             #huom: myös tietyt suomen postpositiot voidaan sulkea pois
                             if(nexttoken2 != FALSE){
                                 #tarkista vielä kahden sanan päähän...
                                 if(!tolower(nexttoken2) %in% vuotias &
                                    !tolower(nexttoken2) %in% c("luvun","годов","luvulta","lukuun","setelin","viikoksi","<viikoksi>","году","года") &
                                    !tolower(nexttoken2) %in% vuosi_tms &
                                    !grepl("vuotis",nexttoken2,ignore.case=T) &
                                    !grepl("ksi>",nexttoken2,ignore.case=T)
                                    ){
                                     #Huom! ei oteta, jos numeraali itse par/gen
                                     #Huom! ignooraa parserivirheet 3:n/6:n -tyyppisissä tapauksissa
                                     if((grepl(part,nextfeat)|nexttoken %in% c("млн","тыс","млн.","тыс."))
                                        & !grepl("\\dn",thistoken)
                                        & !grepl("(Mc--g|Case=Par|Case=Abe)",thisfeat)
                                        & !grepl("maksaneen",sent)
                                        & !grepl("tuomitsi",sent)
                                        & !tolower(thistoken) %in% smallnumbers
                                        ){ 
                                         return(accepted)
                                     }
                                     else{
                                         # Loggaa hylkäystapaukset
                                         write_lines(paste(nexttoken,">>",sent), "/tmp/failed_beacause_CARD_but_NEXT_notPART.txt",append=T)
                                     }
                                 }
                             }
                             else{
                                 #Numeraali on toiseksi viimeinen sana
                                 if((grepl(part,nextfeat)|nexttoken %in% c("млн","тыс","млн.","тыс."))
                                    & !grepl("\\dn",thistoken)
                                    & !grepl("(Mc--g|Case=Par|Case=Abe)",thisfeat)
                                    & !grepl("maksaneen",sent)
                                    & !grepl("prosenttia omistava",sent)
                                    & !tolower(thistoken) %in% smallnumbers
                                    ){ 
                                     return(accepted)
                                 }
                                 else{
                                     # Loggaa hylkäystapaukset
                                     write_lines(paste(nexttoken,">>",sent), "/tmp/failed_beacause_CARD_but_NEXT_notPART.txt",append=T)
                                 }
                             }
                         }
                         else{
                             # Loggaa hylkäystapaukset
                             write_lines(paste(thistoken,">>",sent), "/tmp/failed_beacause_CARD_but_VUOTTA.txt",append=T)
                         }
                     }
                 }
                 else{
                     # Loggaa hylkäystapaukset
                     write_lines(paste(thistoken,">>",sent), "/tmp/failed_beacause_CARD_but_VUOSILUKU.txt",append=T)
                 }
             }
        }
         else{
             #cat("Not checking",thistoken,"\n")
         }
     }

     #Jos ei olla vielä hyväksytty, tarkoittaa sitä, että hylättävä
     return(not_accepted)

}

