
#' Laske, mones sana ajanilmaus on virkkeessä, kun välimerkkejä ei huomioida
#' @param r rivi vaihe1.df-df:stä
#' @export

GetTeNoWithoutPunct <- function(r){
    sent <- r[["sent"]]
    sent <- gsub(" -+ "," ",sent)
    tokens <- unlist(strsplit(sent,"\\s+"))
    return (which(grepl("<",tokens))[1])
}

#' Hae jonkin suhteessa ajanilmaukseen määritellyn sanan morfologiset piirteet
#' @param r rivi vaihe1.df-df:stä
#' @param tepos ajanilmauksen sijainti
#' @param direction kuinka mones sana ja mihin suuntaan (negatiivinen taaksepäin)
#' @export

GetFeatsInRelationToTe <- function(r, tepos, direction){
    #Lisää ajanilmauksen sijaintiin yksi, koska pilkottu posfeat-jono alkaa tyhjällä
    tepos  <- tepos + 1
    #Huom: poista suomenkielisessä annotoinnissa oleva merkintä [psor]=3 tms.
    nopunctfeat <- gsub("\\[(PUNCT|SENT)[^]]+\\]","",gsub("\\[psor\\]","",r[["posfeatlist"]]),ignore.case=T)
    nopunctfeat <- gsub("\\[[-,.!?:;/(]>>[-,.!?:;/)]\\]","",nopunctfeat,ignore.case=T)
    feats <- unlist(strsplit(nopunctfeat,"\\["))
    #show(feats)
    #show(unlist(strsplit(r[["posfeatlist"]],"\\[")))
    if((tepos+direction)<0 | (tepos+direction)>length(feats)){
        return ("")
    }
    else{
        return (feats[tepos + direction])
    }
}




#' Tarkastele, onko ajanilmauksen vieressä paikanilmaus
#' MUTTA rajaa niin, että vain isolla kirjaimella alkavat paikanilmaukset on otettu mukaan
#' TAI lievempänä niin, että jos kaksi sanaa sitten tai kahden sanan päästä alkaa isolla...
#' @param  r dataframen rivi
#' @export
MarkIfHasPlace <- function(r, direction){
    if(direction=="next"){
        #seuraava sana
        next_word_feat <- GetFeatsInRelationToTe(r, GetTeNoWithoutPunct(r), 1)
        second_next_word_feat <- GetFeatsInRelationToTe(r, GetTeNoWithoutPunct(r), 2)
        if(r[["lang"]]=="ru"){
            #Etsi tapauksia, joissa deiktistä adverbiä seuraava sana on prepositio ja sitä seuraava prepositionaalissa
            #oleva substantiivi (entä adjetkiivi?)
            #A....l
            if(grepl("(N...ln|A....l)",second_next_word_feat) & grepl("S>>",next_word_feat)){
                return("yes")
            }
            else{
                return("no")
            }
        }
        else if(r[["lang"]]=="fi"){
            next_word_feat <- GetFeatsInRelationToTe(r, GetTeNoWithoutPunct(r), 1)
            second_next_word_feat <- GetFeatsInRelationToTe(r, GetTeNoWithoutPunct(r), 2)
            if(grepl("Case=(Ade|Ine)",next_word_feat)&!grepl("VerbForm=Part",second_next_word_feat)&!grepl("VERB>>",next_word_feat)){
                return("yes")
            }
            else{
                return("no")
            }
        }
    }
    else{
        #edellinen sana
        if(r[["lang"]]=="ru"){
            prev_word_feat <- GetFeatsInRelationToTe(r, GetTeNoWithoutPunct(r), -1)
            #second_prev_word_feat <- GetFeatsInRelationToTe(r, GetTeNoWithoutPunct(r), -2)
            #Etsi tapauksia, joissa deiktistä adverbiä edeltävä sana on prepositionaalissa
            if(grepl("(N...ln|A....l)",prev_word_feat)){
                return("yes")
            }
            else{
                return("no")
            }
        }
        else if(r[["lang"]]=="fi"){
            prev_word_feat <- GetFeatsInRelationToTe(r, GetTeNoWithoutPunct(r), -1)
            if(grepl("Case=(Ade|Ine)",prev_word_feat)&!grepl("VERB>>",prev_word_feat)){
                return("yes")
            }
            else{
                return("no")
            }
        }
    
    }
}


