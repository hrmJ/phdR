#!/usr/bin/Rscript
#Funktiot aineiston yleistä latausta varten



#' Lataa dataframe kustakin aineistoryhmästä
#' Json-tiedoston perusteella ja tarkista, onko tässä ryhmässä
#' ylimääräisiä tai puuttuvia sarakkeita
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr  %>% select case_when
#' 
#' @export

ProcessJson <- function(path, lang){
    rawd <- as_tibble(fromJSON(path))
    if(nrow(rawd)>0){
        rawd %>% 
            select(tokenid, posfeatlist, sentid, sent, dfunct, headverb, prodrop, headverbdep,
                verbchain, neg, firstlemma, firstpos, firsttoken, phraselength, headverbfeat,
                subjfeat, subjlemma, objfeat, objlemma, objpos, subjpos, subjlength, objlength,
                subjlength2, location, corpus, sourcetext) %>% 
            filter(location != "failed" & location != "Failed" & location != "FAILED") %>% 
            mutate(location = case_when ( 
                                     location == "beforeverb_and_subject" ~ "S1",
                                     location == "beforeverb" ~ "S2",
                                     location == "beforeobject" ~ "S3",
                                     location == "afterobject" ~ "S4",
                                     location %in% c("S1","S2","S3","S4") ~ location
                                     )) %>% 
            mutate(lang=lang, 
                   group=gsub("^([a-zA-Z0-9]+)_.*","\\1",basename(path)),
                   corpustype=case_when(
                                       grepl("araneum",corpus) ~ "araneum",
                                       grepl("press",corpus) ~ "press"
                                       ),
                   location3=case_when(
                                       location %in% c("S2","S3") ~ "S2/S3",
                                       TRUE ~ location
                                       )
                  ) %>% 
            mutate(group=gsub("e6bnew","E6b",group)) %>% 
        return
    }
    else{
        return(rawd)
    }
}


#' Yhdistä ryhmiä, jotka on pitänyt hakea kahtena erillisenä
#' ryhmänä korpuksista
#' @export

AddFixGroups <- function(filtered){
    #Venäjän E3 (в течение последних ... лет)
    filtered$group[filtered$group=="E3bfix"] <- "E3b"
    return(filtered)
}

#' Tee jälkikorjauksia aineistoryhmiin esimerkiksi poistamalla
#' tapauksia, joita aineistossa ei pitäisi olla
#' @export

FixGroupsFi <- function(filtered){
    #Suomen L4a: poistetaan kellonajat ja viikonpäivät (venäjässä jo osin tehty tätä ennen)
    prev.blacklist <- list(
                          paste0("ennen ",c('yhtä', 'kahta', 'kolmea', 'neljää', 'viittä', 'kuutta', 'seitsemää', 'kahdeksaa', 'yhdeksää', 'kymmentä', 'yhtätoista', 'kahtatoista')),
                          paste0(c('yhden', 'kahden', 'kolmen', 'neljän', 'viiden', 'kuuden', 'seitsemän', 'kahdeksan', 'yhdeksän', 'kymmenen', 'yhdentoista', 'kahdentoista')," jälkeen"),
                          paste0(c('yhdestä', 'kahdesta', 'kolmesta', 'neljästä', 'viidestä', 'kuudesta', 'seitsemästä', 'kahdeksasta', 'yhdeksästä', 'kymmenestä', 'yhdestätoista', 'kahdestatoista')," asti"),
                          paste0('(vaille|yli) ', c('yhden', 'kahden', 'kolmen', 'neljän', 'viiden', 'kuuden', 'seitsemän', 'kahdeksan', 'yhdeksän', 'kymmenen', 'yhdentoista', 'kahdentoista')),
                          paste0(c('yhteen', 'kahteen', 'kolmeen', 'neljään', 'viiteen', 'kuuteen', 'seitsemän', 'kahdeksaan', 'yhdeksään', 'kymmeneen', 'yhteentoista', 'kahteentoista')," asti"),
                          c("aikoihin","aikaan","maissa"),
                          paste0("(kello|klo) ",c('yksi', 'kaksi', 'kolme', 'neljä', 'viisi', 'kuusi', 'seitsemän', 'kahdeksan', 'yhdeksän', 'kymmenen', 'yksitoista', 'kaksitoista')),
                          c(" [0-9.:]+", "(kello|klo) [0-9.:]+",":n jälkeen"),
                          c("(tammi|helmi|maalis|huhti|touko|kesä|heinä|elo|syys|loka|marras|joulu)kuuta"),
                          c("(maanantaina|tiistaina|keskiviikkona|torstaina|perjantaina|lauantaina|sunnuntaina)")
                        )

    next.blacklist <- list(
                           paste0(c('yhden', 'kahden', 'kolmen', 'neljän', 'viiden', 'kuuden', 'seitsemän', 'kahdeksan', 'yhdeksän', 'kymmenen', 'yhdentoista', 'kahdentoista')," (jälkeen|maissa|aikoihin|aikaan)"),
                           c("(vähän )?ennen","(\\d+:n|[a-ö]+n) jälkeen"),
                           c(" [0-9.:]+"),
                           c("(noin |ennen kello |ennen klo)?(kello|klo)"),
                           c("[a-ö-]+ [0-8.:]+"),
                           c("[a-ö0-9.:-]+ (tammi|helmi|maalis|huhti|touko|kesä|heinä|elo|syys|loka|marras|joulu)kuuta"),
                           c("(viime|toissa|tänä|ensi|seuraavana|tulevana )?(maanantaina|tiistaina|keskiviikkona|torstaina|perjantaina|lauantaina|sunnuntaina)")
                           )
    for (thislist in prev.blacklist){
        for (item in thislist){
            filtered <- subset(filtered,!grepl(paste0(item, " <"),filtered$sent, ignore.case=T))
        }
    }


    for (thislist in next.blacklist){
        for (item in thislist){
            filtered <- subset(filtered,!grepl(paste0("> ", item),filtered$sent, ignore.case=T))
        }
    }

    return(filtered)
}

#' Tee jälkikorjauksia aineistoryhmiin esimerkiksi poistamalla
#' tapauksia, joita aineistossa ei pitäisi olla
#' @export

FixGroupsRu <- function(filtered){
    prev.blacklist <- list( c("понедельник","вторник","среду","четверг","пятницу","субботу","воскресенье"))
    next.blacklist <- list( paste0("в ", c("понедельник","вторник","среду","четверг","пятницу","субботу","воскресенье")))
    for (thislist in prev.blacklist){
        for (item in thislist){
            filtered <- subset(filtered,!grepl(paste0(item, " <"),filtered$sent, ignore.case=T))
        }
    }
    for (thislist in next.blacklist){
        for (item in thislist){
            filtered <- subset(filtered,!grepl(paste0("> ", item),filtered$sent, ignore.case=T))
        }
    }
    return(filtered)
}

#' Vaihda vanhantyyppinen ryhmän nimi uudeksi
#' @export

FormatGroupName <- function(mydf){
    conv.table <- read.csv(system.file("extdata", "groupnameconversion.csv", package="phdR2"))
    for(gname in unique(mydf$group)){
        if(gname %in% conv.table$oldname){
            newname <- as.character(conv.table$newname[conv.table$oldname==gname])
            mydf$group[which(mydf$group==gname)] <- newname
        }
    }
    return(mydf)
}

#' Poista datasta ylimääräisiä: esimerkiksi kieltolauseet
#' 
#' @importFrom dplyr  %>% filter
#' 
#' @export

FilterOut <- function(filtered){
    #ei mukaan sellaisia, joiden sijaintia ei onnistuttu määrittämään
    filtered <- filtered[filtered$location!="failed",]
    #ei mukaan sellaisia, joissa subjekti verbissä
    filtered <- filtered[filtered$prodrop=="No",]
    #ei mukaan kieltolauseita
    filtered <- filtered[filtered$neg=="no",]
    #ei mukaan ryhmiä F5(koskaan) ja P3 (enää)
    filtered <- filtered[!filtered$group %in% c('F5','P3'),]
    #ei mukaan kaunokirjallisia
    filtered <- filtered[!grepl('fiction',filtered$corpus),]
    #Korjataan automaattista analyysiä:
        filtered <- filtered[!grepl('<siitä> asti,? kun',filtered$sent, ignore.case=T),]
        filtered <- filtered[!grepl('<siitä> asti,? ku ',filtered$sent, ignore.case=T),]
        # filtteröidään pois tapauksia, joissa pääverbi intransitiivinen tai muuta hassua
        # TODO: selitä näiden poisjättöä tekstissä
        filtered <- filtered[!filtered$headverb %in% c("muuttua","kasvaa","nousta","vähetä","supistua","alkaa","syntyä","kiertyä","kuulua","olla","paukkua","loistaa","liplattaa","raikua","piristä","käynnistyä","jatkua","lisääntyä"),]
        filtered <- filtered[!grepl('(miten|kuinka) *<kauan>',filtered$sent, ignore.case=T),]
        filtered <- filtered[!grepl('(как) *<долго>',filtered$sent, ignore.case=T),]
        #väärin määritellyn sijainnin filtteröintiä
        filtered <- filtered[!grepl('<maissa> (lähestyimme|helpotti|pitää|aloittivat|sain)', filtered$sent),]
        filtered <- filtered[!grepl('<paikkeilla> saan pitkän', filtered$sent),]
        #Filtteröi l4a:sta pois "однажды" ja kerran
        filtered <- filtered[!(grepl("(однажды|kerran) <",filtered$sent,ignore.case=T) & filtered$group=="L4a"),]
        #E1a (venäjä): tietyt pääverbit kiellettyjä
            fcon  <- file(system.file('extdata','e1headverbs.txt',package='phdR2'))
            headblacklist  <- readLines(fcon)
            close(fcon)
            filtered <- filtered[!(filtered$headverb %in% headblacklist & filtered$group=="E1a"),]
        #venäjän E1b + E5a, jotka eivät edusta E-funktiota
        filtered <- filtered %>% filter(!(group %in% c("E1b","E5a") & lang=="ru" &
                       grepl("<за> ([а-я0-9]+ ){1,3}(до|после) ", sent,ignore.case=T))) %>% 
                                filter(!(group %in% c("E1b","E5a") & lang=="ru" &
                                                       grepl("впервые <за> ", sent,ignore.case=T)))
        #venäjän LM1-ryhmästä pois раз-sanat
        filtered <- filtered %>% filter(!(group=="LM1" & grepl("> [а-я]+ раз",sent)))
        #Poistetaan suomen L5a:sta ylimääräiset monikot ja "päivänä" -sana
        filtered <- filtered  %>% filter(!(lang=="fi" & group=="L5a" & !grepl("<vuonna",sent,ignore.case=T)))
        #Poistetaan venäjän L9b:stä впервые
        filtered <- filtered  %>% filter(!(lang=="ru" & group=="L9d" & grepl("впервые <с",sent,ignore.case=T)))
        # Korjataan suomen E6b:stä pois pitkään aikaan ja pitkiin aikoihin
        filtered <- filtered  %>% filter(!(lang=="fi" & group=="E6b" & grepl("(<aikaan|<aikoi|pitkään> aikaan|pitkään aikaan)",sent,ignore.case=T)))
        # Korjataan suomen duratiivisista pois viettää ja kestää
        filtered <- filtered  %>% filter(!(lang=="fi" & grepl("E",group) & headverb %in% c("viettää","kestää")))
        # Korjataan E1a-ryhmistä tuntia viikossa tms.
        filtered <- filtered  %>% filter(!(group=="E1a" & grepl("> (в|за) (недел|ден|год|сутках)",sent, ignore.case=T)))
        filtered <- filtered  %>% filter(!(group=="E1a" & grepl("> (viikossa|vuodessa|päivässä)",sent, ignore.case=T)))
        # Korjataan venäjän E1a-ryhmistä каждые x лет ym.
        filtered <- filtered  %>% filter(!(group=="E1a" & grepl("каждые [а-я]+ <",sent, ignore.case=T)))
        


    return(filtered)
}

#' Lisää uusia muuttujia, joita ei tietokannassa ollut
#' 
#' @import dplyr
#' 
#' @export

AddVariables <- function(mydf){
    read.csv(system.file("extdata", "group_properties_fi.csv", package="phdR2"))  %>% 
        mutate(lang="fi") %>% rbind(.,
    read.csv(system.file("extdata", "group_properties_ru.csv", package="phdR2")) %>% 
        mutate(lang="ru"))  %>% 
    rename(group=name) %>% 
    mutate_if(is.factor, as.character) -> tab

    mydf %>% 
        mutate_if(is.factor, as.character) %>% 
        left_join(tab,by=c("lang","group"))  %>% 
        return
}


#' Muuta joidenkin muuttujien määrittelyä
#' @export

FixVariables <- function(mydf){
    #Korjaa FUNCT-kategorioita väljemmiksi
    mydf$funct[mydf$funct %in% c('keh','punkt')] <- 'sim'
    mydf$funct[mydf$funct %in% c('fkalend','f')] <- 'freq'
    mydf$funct[mydf$group %in% c('E7')] <- 'tapa'
    mydf$funct[mydf$group %in% c('E3a','E3b')] <- 'res.keh'
    mydf$funct[mydf$group %in% c('E1b','E4','E5a','E5b')] <- 'res.vars'
    mydf$subjpos[mydf$subjpos %in% c("P","PRON")] <- "pron"
    mydf$subjpos[mydf$subjpos!="pron"] <- "muu"
    mydf$subjpos <- as.factor(mydf$subjpos)
    mydf$objpos[mydf$objpos %in% c("P","PRON")] <- "pron"
    mydf$objpos[mydf$objpos!="pron"] <- "muu"
    mydf$morph[mydf$ref=="UT" & mydf$morph=="ADV"] <- "deict.ADV"
    mydf$morph[mydf$group=="L5b"] <- "deict.ADV"
    #Nyt kun suomen L6b on myös adv..
    mydf$morph[mydf$group=="E6b"] <- "ADV"

    mydf$group[mydf$group=="L5b"] <- "L5c"
    mydf$group[mydf$group=="L5a" & grepl("(viikolla>)|(на> [а-яА-Я]+ неделе)",mydf$sent,ignore.case=T)] <- "L5b"


        mydf$clausestatus <- 'muu'
        ru.suboords <- c('что','если','хотя','чтобы','пока','поскольку','коль','ежели')
        fi.relpron <- c('joka','mikä')
        ru.relpron <- c('который','что','кто')
        qwords <- c("где", "зачем", "как", "какой", "каков", "когда", "который", "кто", "куда", "откуда", "почему", "сколько", "чей","kuinka","missä", "miksi", "miten", "millainen", "milloin", "koska", "kumpi", "kuka", "minne", "mistä", "kenen", "minkä")

        # Alisteiset
        # ----------
        # Suomen alisteiset suoraan parserilta, Venäjän alisteiset listan perusteella:
        # Huom! sillä että määritellään myös sanaluokka, pyritään erottelemaan
        # relatiivi-что ja konjunktio-что

        mydf$clausestatus[mydf$firstpos=="SCONJ" | (mydf$firstlemma %in% ru.suboords & mydf$firstpos=='C')] <- 'suboord'
        #Relatiiviset
        mydf$clausestatus[mydf$firstlemma %in% fi.relpron | (mydf$firstlemma %in% ru.relpron & mydf$firstpos == 'P')] <- 'rel'
            #Preposition sisältävät relatiiviset:
            mydf$clausestatus[which(grepl("[ ,](о|из|для|ради|после|за|в|на|c|до|перед|возле|рядом|у|без|вопреки|навстречу|благодаря|через|вдоль|по) (ком |кого |кем |чем |чего |котор|какие |каких ) ?[^\\.\\,\\!\\?\\:\\;]+<",mydf$sent))] <- "rel"
            #Korjataan suomesta Joka <vuosi> -tyyppiset
            mydf$clausestatus[mydf$lang=="fi" & mydf$clausestatus=="rel" & mydf$location=="beforeverb_and_subject" & (mydf$group %in% c("F1a") | mydf$firsttoken %in% c("joka","jotka","mikä","Joka","Jotka"))] <- 'muu'

        #Jaotellaan alisteisia
        mydf$clausestatus[mydf$firstlemma %in% c("jos","kun","если","когда")] <- 'jos/kun'
        mydf$clausestatus[mydf$firstlemma=="että" | mydf$firstlemma=="что" & mydf$clausestatus!="rel"] <- 'että'

        mydf$clausestatus[mydf$firstlemma %in% qwords & (is.na(mydf$clausestatus) | mydf$clausestatus=="muu")] <- "es.kys"
        mydf$clausestatus[mydf$firstlemma %in% c("vaikka","хотя") | mydf$clausestatus %in% c("suboord", "es.kys")] <- 'muu sivulause'
        mydf$clausestatus[mydf$clausestatus %in% c("että","jos/kun","muu sivulause")] <- "suboord"

        #Kysymyssanat
        mydf$clausestatus[grepl(">[^\\.\\,\\!\\?\\:\\;]+\\?",mydf$sent)] <- "kys"

        #Rinnasteiset lauseet
        #Muista perustella, miksei mukana "а"
        mydf$clausestatus[mydf$firstlemma %in% c("но", "mutta")] <- 'mutta'

    #Kategorisoidaan subjektin pituus

    mydf$subjlength2<- as.integer(mydf$subjlength2)
    mydf$objlength<- as.integer(mydf$objlength)

    mydf$subjtype <- "short"
    mydf$subjtype[mydf$subjlength2>1] <- "long"
    mydf$subjtype[mydf$subjpos=="pron"] <- "short"

    #Kategorisoidaan objektin pituus

    mydf$objlength[mydf$objlength<1] <- 1
    mydf$objtype <- "short"
    mydf$objtype[mydf$objlength>1] <- "long"
    mydf$objtype[mydf$objpos=="pron"] <- "short"


    #Korjataan referentialisuuden  määrittelyä

    mydf$ref[mydf$group %in% c('L5a','L2a','L2b','L9a','L9b','L9c','L9d')] <- 'abs'
    mydf$ref[mydf$group %in% c('E3b','E2b')] <- 'subj'
    mydf$ref[mydf$group %in% c('L6b')] <- 'obj'
    mydf$ref[mydf$group %in% c('L6a')] <- 'TT'
    mydf$ref[grepl('(viime|прошл[а-я]+|послед[а-я]+) ([a-ö]+|[а-я]+) <',mydf$sent, ignore.case=T) & mydf$group=='E3b'] <- 'abs'
    #Radikaalisti simppelimpi:
    mydf$ref[mydf$ref %in% c("abs","TT","obj","subj")] <- 'muu'


    return(mydf)
}

#' Apufunktio semanttisen funktion ym. määrittelyyn
#' @export

DefineGroupProperties <- function(name,propname, conv.table){
    conv.table <- system.file("extdata", "groupnameconversion.csv", package="phdR2")
    fi.conv.table <- read.csv(system.file('extdata','group_properties_fi.csv', package="phdR2"))
    ru.conv.table <- read.csv(system.file('extdata','group_properties_ru.csv', package="phdR2"))
    for(gname in unique(mydf$group)){
        mydf
        newname <- as.character(conv.table$newname[conv.table$oldname==gname])
        mydf$group[which(mydf$group==gname)] <- newname
    }
    return(mydf)
    return(as.character(conv.table[[propname]][conv.table$name==name]))
}

#' Muuta faktoreiksi kaikki tarvittavat
#' @export

Factorize <- function(mydf){
    mydf[c("morph","funct","pos")] <- lapply(mydf[c("morph","funct","pos")], as.factor)
    mydf$location  <-  factor(mydf$location, levels=c("beforeverb_and_subject","beforeverb","beforeobject","afterobject"), labels=c("S1","S2","S3","S4"))
    mydf$location3 <- as.character(mydf$location)
    mydf$location3[mydf$location %in% c("S2","S3")] <- "S2/S3"
    mydf$location3 <- as.factor(mydf$location3)
    return(mydf)
}

#' Tee vielä pari muokkausta 
#' @export

FinalConversions <- function(mydf){
    mydf$corpustype[grepl('araneum',mydf$corpus)] <- "araneum"
    mydf$corpustype[grepl('press',mydf$corpus)] <- "press"
    return(mydf)
}



