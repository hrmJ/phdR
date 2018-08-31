#' muokkaa kaikkien lauseen sanojen morfologisen rakenteen sisältävän merkkijonon tietoja
#' HUOM! Tämä on spesifisti ajatellen kysymystä siitä, seuraako ajanilmausta jokin numeraali!
#'
#' @export

FormatFeatString <- function(thisrow){
    margins <- DetermineAdvPhraseLength(thisrow)
    s <- RemoveAdvFeat(s,thisrow["tokenid"],margins$addedmargin, margins$substractedmargin, thisrow["sentid"])
    return (s)
}


#' Poista piirresarjasta ajan adverbin piirteet ja lyhennä kattamaan vain ympäröivä lause
#'
#' Piirresarjalla tarkoitetaan virkkeen sanojen morfologisia ominaisuuksia, jotka on kirjoitettu jonoksi hakasulkeiden väliin. Tämä funktio poistaa ajanilmauksen piirteet piirresarjasta ja pyrkii myös rajaamaan käsiteltävän alueen vain ajanilmauksen sisältäväksi lauseeksi välimerkkien perusteella.
#'
#' @param s kokonainen, muokkaamaton piirresarja
#' @param tokenid kuinka mones sana ajanilmauksen ensimmäinen sana on 
#' @param addedmargin kuinka monta sanaa ajanilmaukseen kuuluu ensimmäisen sanan jälkeen
#' @param substractedmargin kuinka monta sanaa ajanilmaukseen kuuluu ennen ensimmäistä sanaa
#' @param sentid lauseen id (debugging only)
#'
#' @export

RemoveAdvFeat <- function(s,tokenid,addedmargin,substractedmargin, sentid=NULL){
    word <- "(\\[[^]]+\\])"
    words_from_start_to_last_word_of_adv <- as.integer(tokenid) - 1 - substractedmargin
    keep_before_adv <- paste0("(",word,"{", words_from_start_to_last_word_of_adv, "})")
    adv_and_margin <- paste0(word,"{",substractedmargin + addedmargin + 1,"}")
    keep_after_adv  <- paste0("(",word,"*",")")
    search_string   <- paste0(keep_before_adv,adv_and_margin,keep_after_adv)
    before <- sub(search_string,"\\1",s)
    after <- sub(search_string,"\\4",s)
    #poista pilkkua tai muuta välimerkkiä edeltävät sanat
    before <- gsub(".*\\[PUNCT>>[^]]*\\]","",before)
    before <- gsub(".*\\[,>>,\\]","",before)
    before <- gsub(".*\\[S>>SENT\\]","",before)
    #HUOMIO ainoastaan ilmaukset *ensimmäiseen vastaantulevaan pilkkuun tai muuhun välimerkkiin* asti
    s <- paste0(before,after)
    s <- gsub("\\[PUNCT>>.*","",s)
    s <- gsub("\\[,>>,.*","",s)
    s <- gsub("\\[,>>,.*","",s)
    s <- gsub("\\[S>>SENT.*","",s)
    return(s)
}


#' Määrittele, kuinka pitkä on ajanilmauksen sisältämä lauseke
#'
#' @importFrom stringr str_extract
#' @return lista, jonka arvoina lausekkeen pituus molempiin suuntiin (substractedmargin=x,addedmargin=x)
#'
#' @export

DetermineAdvPhraseLength <- function(thisrow, feats_vector=c()){
    if(thisrow["lang"]=="ru"){
        s <- thisrow["posfeatlist"]
        if(thisrow["funct"]=="res.keh"){
            #venäjän v tetshenije -tapaukset ovat erityisasemassa, koska pituus vaihtelee
            substractedmargin  <-  0
            vtetshenije_pat <- "\\[S>>Sp-a\\]\\[N>>Ncnsan\\]"
            words_between_pat <- "((\\[[^]]+\\]){0,4})" #oleta maksimimääräksi saneita ennen genetiiviä neljä
            gen_pat <- "\\[N>>Ncm[ps]gn\\]"
                       "\\[N>>Ncm[ps]gn\\]"
            pat <- paste0(vtetshenije_pat,words_between_pat,gen_pat)
            advphrase <- str_extract(s[[1]],pat)
            addedmargin <- length(feats_vector) - 1 
        }
        else{
            gconv <- read.csv(system.file("extdata", "groupnameconversion.csv", package="phdR"))
            gconv$newname <- as.character(gconv$newname)
            gconv$oldname <- as.character(gconv$oldname)
            nn <- as.character(gconv$newname)
            g <- as.character(unlist(thisrow["group"]))
            on <- gconv$oldname[which(nn==g)[1]][1]
            ldf <- data.frame(mar=c(0, 0, 0, 2, 0, 1, 2, 2, 2, 2, 0, 0, 0, 0, 2, 2, 2, 2, 2, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 2, 3, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2,0,1), 
                              on= c('lc0a', 'lc0b', 'lc0c', 'lc1' , 'lc2' , 'lc3' , 'lc4' , 'lc5' , 'lc6' , 'lc7a', 'lc7b', 'lc8' , 'lc9a', 'lc9b', 'lc10', 'lc11', 'lc12', 'lc13', 'lc15', 'lc16', 'lc17', 'fr1' , 'fr2' , 'fr3a', 'fr3b', 'fr4a', 'fr4b', 'fr5a', 'fr5b', 'fr6' , 'fr7' , 'fr8' , 'ex1' , 'ex1b', 'ex2a', 'ex2b', 'ex3a', 'ex3b', 'ex4' , 'ex5a', 'ex5b', 'ex6' , 'ex7' , 'ex8' , 'ex9' , 'ex10', 'ex11', 'pr1' , 'pr2' , 'pr3' , 'lm1a', 'lm1b','lc1b', 'jp1'),
                              substrmar=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,0))
            addedmargin <- ldf$mar[which(ldf$on==on)[1]][1]
            substractedmargin <- ldf$substrmar[which(ldf$on==on[1])][1]
            if(thisrow["group"]=="L5c"){
                addedmargin <- 0
                substractedmargin <- 0
            }
            if(thisrow["group"]=="LM1"){
                addedmargin <- 2
                substractedmargin <- 0
            }
        }
    }
    else if(thisrow["lang"]=="fi"){
        addedmargin <- ifelse(thisrow["group"] %in% c('L6b', 'L7a', 'L6a', 'L9a', 'L9d'),1,0)
        substractedmargin <- 0
        if(thisrow["group"] %in% c("E3b","L9b")){
            substractedmargin <- 2
        }
        if(thisrow["group"] %in% c("l5a", "L7b", "L4b", "L7d", "F1a", "F2a", "E1a", "E1b", "E4", "E3a", "E1c", "E6b", "E6c", "E2b", "LM1")){
            substractedmargin <- 1
        }
    }
    return (list(addedmargin=addedmargin,substractedmargin=substractedmargin))
}


#' Poista piirresarjasta ajan adverbin piirteet
#'
#' Piirresarjalla tarkoitetaan virkkeen sanojen morfologisia ominaisuuksia, jotka on kirjoitettu jonoksi hakasulkeiden väliin. Tämä funktio poistaa ajanilmauksen piirteet piirresarjasta ja pyrkii myös rajaamaan käsiteltävän alueen vain ajanilmauksen sisältäväksi lauseeksi välimerkkien perusteella.
#'
#' @param s kokonainen, muokkaamaton piirresarja
#' @param tokenid kuinka mones sana ajanilmauksen ensimmäinen sana on 
#' @param addedmargin kuinka monta sanaa ajanilmaukseen kuuluu ensimmäisen sanan jälkeen
#' @param substractedmargin kuinka monta sanaa ajanilmaukseen kuuluu ennen ensimmäistä sanaa
#' @param sentid lauseen id (debugging only)
#'
#' @export

RemoveAdvFeat <- function(s,tokenid,addedmargin,substractedmargin, sentid=NULL){
    word <- "(\\[[^]]+\\])"
    words_from_start_to_last_word_of_adv <- as.integer(tokenid) - 1 - substractedmargin
    keep_before_adv <- paste0("(",word,"{", words_from_start_to_last_word_of_adv, "})")
    adv_and_margin <- paste0(word,"{",substractedmargin + addedmargin + 1,"}")
    keep_after_adv  <- paste0("(",word,"*",")")
    search_string   <- paste0(keep_before_adv,adv_and_margin,keep_after_adv)
    before <- sub(search_string,"\\1",s)
    after <- sub(search_string,"\\4",s)
    #poista pilkkua tai muuta välimerkkiä edeltävät sanat
    before <- gsub(".*\\[PUNCT>>[^]]*\\]","",before)
    before <- gsub(".*\\[,>>,\\]","",before)
    before <- gsub(".*\\[S>>SENT\\]","",before)
    #HUOMIO ainoastaan ilmaukset *ensimmäisen vastaantulevaan pilkkuun tai muuhun välimerkkiin* asti
    s <- paste0(before,after)
    s <- gsub("\\[PUNCT>>.*","",s)
    s <- gsub("\\[,>>,.*","",s)
    s <- gsub("\\[,>>,.*","",s)
    s <- gsub("\\[S>>SENT.*","",s)
    return(s)
}

