
#' Hakee joka sijainnista n määrän esimerkkejä (jos löytyy)
#'
#' @param groupsubset yhden kielen yhden aineistoryhmän kaikki lauseet (df)
#' @param thisloc sijainti (S1-S4)
#' @param sample_size kuinka monta lausetta kustakin sijainnista halutaan
#' @return satunnaisotanta lauseita
#' @import dplyr
#' @export

GetSamplesForPos  <- function(thisloc, groupsubset, sample_size){
    pos_subset  <- groupsubset %>%  filter(location == thisloc) 
    pos_subset %>% 
        pull(sent)  %>% 
        sample(.,ifelse(nrow(pos_subset)>=sample_size,
                        sample_size,
                        nrow(pos_subset)))  %>% 
        return
}

#' Hakee joka ryhmästä n satunnaista esimerkkiä
#' 
#' @param g aineistoryhmän nimi
#' @param langsubset yhden kielen kaikki lauseet (df)
#' @param sample_size kuinka monta lausetta kustakin sijainnista halutaan
#' @return tästä ryhmästä haetut esimerkkilauseet sijainneittain järjestettynä listana
#' 
#' @import dplyr
#' @export

GetSamplesForGroup  <- function(g, langsubset, sample_size){
   cat(g,"\n")
   langsubset  %>% filter(group==g) -> this_d
   lapply(unique(this_d$location), GetSamplesForPos, groupsubset=this_d, sample_size=sample_size)  %>% 
       setNames(unique(this_d$location)) %>% 
       return
}

#' Hakee joka kielestä jokaisen aineistoryhmän ja joka aineistoryhmästä n esimerkkiä
#' 
#' @param l kielen nimi
#' @param alldata koko aineistoa edustava dataframe, josta lähdetään liikkeelle
#' @param sample_size kuinka monta lausetta kustakin sijainnista halutaan
#' 
#' @import dplyr
#' @export

GetSamplesForLang <- function(l, alldata, sample_size){
    cat("\n", l,"\n")
    alldata %>% filter(lang == l)  -> d
    mylist <- list()
    for(g in unique(d$group)){
        mylist[[g]] <- GetSamplesForGroup(g,d, sample_size=sample_size)
    }
    return (mylist)
}



#' Haetaan esimerkkejä parserin tarkkuuden määrittelemiseksi
#' 
#' 
#' @param sample_size kuinka monta lausetta kustakin sijainnista halutaan
#' @param alldata data, josta lähdetään liikkeelle (df)
#' @return tibble, jossa testit
#' 
#' @import dplyr
#' @export

BuildAccuracyTest <- function(alldata, sample_size){
    lapply(researchdata$langs,GetSamplesForLang, alldata=alldata, sample_size=sample_size) %>% 
        setNames(researchdata$langs)  %>% 
        melt  %>% 
        as_tibble %>% 
        mutate_if(is.factor,as.character)   %>% 
        mutate(correct="not checked")  %>% 
        dplyr::rename("sent"=value,"group"=L2,"lang"=L1,"location"=L3) %>% 
        return 
}


#' Kysyy määritelmää jollekin esimerkille tms.
#' 
#' Käytetään yleensä apply-funktion callbackinä manuaalista annotointia tms. varten
#' 
#' @param r yksi dataframen rivi
#' @param cols_to_show vektori, jossa on niitten sarakkeiden nimet, joita halutaan näyttää päätöksenteon pohjaksi
#' @param backup_file polku tiedostoon, jonne jokainen vastaus tallennetaan varmuuskopiona
#' @return Käyttäjän kirjoittama merkkijono
#' 
#' @import dplyr
#' @importFrom readr write_lines
#' 
#' @export

CheckSample_df <- function(r, cols_to_show, backup_file="/home/juho/drive/backups/backup_for_checksample.txt"){
    content  <- sapply(r[cols_to_show],function(x) paste(strwrap(x, 79),collapse="\n"))
    cat("\n\n", paste(cols_to_show,content,sep="\n=====\n",collapse="\n\n"),"\n\n")
    def <- readline("\nMäärittele:\n")
    write_lines(paste0(
                       paste(r[cols_to_show],collapse="|"),
                       "|",def)
                ,backup_file,append=T)
    return(def)
}



#' Kysyy määritelmää jollekin esimerkille tms.
#' 
#' Käytetään yleensä sapply-funktion callbackinä manuaalista annotointia tms. varten
#' 
#' @param show_this elementti, joka käyttäjälle näytetään päätöksenteon pohjaksi
#' @param backup_file polku tiedostoon, jonne jokainen vastaus tallennetaan varmuuskopiona
#' @return Käyttäjän kirjoittama merkkijono
#' 
#' @importFrom readr write_lines
#' 
#' @export

CheckSample_simple <- function(show_this, backup_file="/home/juho/drive/backups/backup_for_checksample.txt"){
    cat("\n\n", paste(strwrap(show_this, 80), collapse="\n"), "\n\n")
    def <- readline("Määrittele:")
    write_lines(paste0(show_this,"|",def),backup_file,append=T)
    return(def)
}


#' Hae määritellystä osa-aineistoista viisi (tai niin paljon kuin löytyy) satunnaista esimerkkiä ja tulosta ne lähteineen
#' @param subsetted tutkimusaineiston osa, josta haetaan
#'
#' @export

GiveExample <- function(subsetted){
    numberofexamples <- ifelse(nrow(subsetted)>=5,5,nrow(subsetted))
    rows <- sample(c(1:nrow(subsetted)),numberofexamples)
    samplelist <- ""
    for (row in rows){
        samplelist <- paste(samplelist,row,": ",gsub("[<>]","",subsetted[row,"sent"]), " (", FormatExampleSource(subsetted[row,]), ")\n", sep="")
    }
    cat(samplelist)
    show(nrow(subsetted))
}


#' Muotoile esimerkin lähde raakadatasta luettavampaan muotoon
#' @param exrow dataframen rivi, jossa lause ja lähde
#'
#' @export

FormatExampleSource <- function(exrow){
    st <- as.character(exrow[["sourcetext"]])
    if(exrow["corpustype"] == "press" & exrow["lang"]=="fi"){
        if(grepl("_[a-ö -]+_",st,ignore.case=T)){
            return(paste("FiPress:",gsub("[^_]*_([a-ö -]+)_.*","\\1",st, ignore.case=T)))
        }
        else if(grepl(" no\\. ",st,ignore.case=T)){
            return(paste("Fipress:",gsub("([a-ö -]+) no\\. .*","\\1",st, ignore.case=T)) )
        }
    }

    if(exrow["corpustype"] == "press" & exrow["lang"]=="ru"){
        if(grepl("\\/\\/",st,ignore.case=T)){
            return(paste("RuPress:",gsub(".*\\/\\/ ([^,]+).*","\\1",st, ignore.case=T)))
        }
    }

    if(exrow["corpustype"] == "araneum" & exrow["lang"]=="fi"){
        if(grepl("\\.\\.\\.",st,ignore.case=T)){
            return(paste("Araneum Finnicum:",gsub(".*\\.\\.\\.(.*)","\\1",st, ignore.case=T)))
        }
        else{
            return(paste("Araneum Finnicum:",st))
        }
    }

    if(exrow["corpustype"] == "araneum" & exrow["lang"]=="ru"){
        if(grepl("\\.\\.\\.",st,ignore.case=T)){
            return(paste("Araneum Russicum:",gsub(".*\\.\\.\\.(.*)","\\1",st, ignore.case=T)))
        }
        else{
            return(paste("Araneum Russicum:",st))
        }
    }

    return(st)
}

