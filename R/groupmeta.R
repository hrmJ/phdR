
#' 
#' Tulostaa taulukon, jossa haluttujen aineistoryhmien ominaisuudet on listattu tarkemmin
#' 
#' @param gnames tulostettavien aineistoryhmien nimet
#' @param caption taulukon otsikko
#' 
#' @importFrom dplyr %>% filter mutate select
#' @importFrom kableExtra kable
#' 
#' @export

GetGroupMetaTab <- function(gnames){
    cap_ending <- " tarkemmat ominaisuudet"
    if(length(gnames)>1){
        cap  <- paste0("Ryhmien ", 
                       paste(gnames[1:length(gnames)-1], collapse=", "),
                       " ja ",
                       gnames[length(gnames)],
                       cap_ending
                       )
    }
    else{
        cap <- paste0("Ryhmän ", gnames[1], cap_ending)
    }


    groupmeta %>% filter(group %in% gnames) %>% 
        mutate(ref=case_when(ref=="UT" ~ "deikt.", ref == "anaf" ~ "anafor.", TRUE ~ ref)) %>% 
        mutate(funct = paste(substr(group,1,1),funct,sep="/")) %>% 
        select(Ryhmä=group,
               Takson.1=tax1, 
               Takson.2=funct,
               Takson.3=ref,
               `n/fi`,
               `n/ru`,
               #`Morfologinen rakenne`=morph,
               ) %>%
        kable(caption=cap,  booktabs=T)  %>% 
        kable_styling(latex_options=c("HOLD_position"),full_width = T) %>% 
        column_spec(1, width="1.2cm")  


}


#' 
#' Tallentaa aineistoryhmien taksonomioiden 1--3 mukaiset ominaisuudet tibbleksi
#' 
#' @importFrom dplyr  %>% filter as_tibble case_when rename count select mutate left_join 
#' @importFrom tidyr spread
#' 
MakeGroupMetaTibble <- function(){

    d  %>% pull(group)  %>% unique %>% as_tibble %>% dplyr::rename(group=value)   %>% 
        left_join(d %>% count(group,funct,pos,morph, ref)  %>% select(group,funct,pos,morph, ref),
                  by="group") -> groupmeta
    groupmeta <- groupmeta %>% 
        mutate(tax1 = case_when(
                                group %in% c("L1a","L1b","L1c") ~ "deikt. päivännimet",
                                group %in% c("L6a","L6b","L7a") ~ "adpos",
                                group %in% c("L9a","L9d","E3a","E3b","L5a","F1a","E1a","E1b","E1c") ~ "luonnon sykl.",
                                group %in% c("E2a","E2b","L3","L5b","L4b","LM1") ~ "edell. johdetut",
                                group %in% c("E4","E3b","E5a","E6c","L7b","L7d","L5c","F4") ~ "aikaa kvant.", 
                                group %in% c("F2a","L7a") ~ "muu ei-kalend.",
                                morph %in% c("deict.ADV","ADV") ~ "adv/pron", 
                                pos == 1 ~ "pos",
                                TRUE ~ "?"))  %>% 
        mutate(tax1 =
               factor(tax1,levels=rev(c("pos","deikt. päivännimet", "luonnon sykl.", "edell. johdetut",
                                    "aikaa kvant.","adpos","adv/pron","muu ei-kalend.")))) %>% 
        mutate(funct = factor(funct,levels=c("sim",  "sekv", "dist","sekv-dur", "likim", "freq",
                                             "dur",  "telic", "res.keh", "res.vars", "tapa",
                                             "presup")))  %>% 
        left_join(., d %>% count(group, lang)  %>% spread(key=lang,value=n),by="group") %>% 
        mutate(morph=as.character(morph)) %>% 
        mutate(group_with_ref=case_when(ref == "muu" ~ paste0(group,"*"),
                               ref == "UT" ~ paste0(group,"**"),
                               ref == "anaf" ~ paste0(group,"***"),
                               TRUE~group
                               ),
               n = fi+ru,
               morph=case_when(morph == "deict.ADV" ~ "ADV",
                               TRUE~morph
                               )

               ) %>% 
        rename(`n/fi`=fi,`n/ru`=ru)
        
        save(groupmeta, file="~/workprojects/phdR2/data/groupmeta.rda")


}

