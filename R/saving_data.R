
#' Luo nopeamman viittauksen mahdollistavia shortcut-tibblejä
#' 
#' @importFrom pbapply pblapply
#' @export

CreateGtabs <- function(){

        #Ristiintaulukointeja prosenttiosuuksilla aineistoryhmittäin ja koko aineiston laajuisesti
        gtabs <- lapply(unique(as.character(d$group)),function(x,df) return(list(counts=xtabs(~lang + location3, df,group==x), props=100*prop.table(xtabs(~lang + location3, df,group==x),1))),df=d)
        names(gtabs) <- unique(as.character(d$group))
        gtabs$all <- list(props=100*prop.table(xtabs(~lang + location3, d),1), counts=xtabs(~lang + location3, d))

        #Aineistoryhmäkohtaisia sijainneittain jaoteltuja dataframejä
        g.df <- GroupSubsets(unique(d$group))
        g.df.simp <- setNames(pblapply(unique(d$group),function(x,v1df){ tmp=subset(v1df,group==x);tmp$group<-as.character(tmp$group); return(tmp)},v1df=d),unique(d$group))
        save(gtabs,file="~/workprojects/phdR2/data/gtabs.rda")
        save(g.df,file="~/workprojects/phdR2/data/g.df.rda")
        save(g.df.simp,file="~/workprojects/phdR2/data/g.df.simp.rda")
}

#' Luo koko tutkimusdatasta aineistoryhmäkohtaisia alijoukkoja. 
#' 
#' @param groupname lista ryhmistä, joista alijoukot luodaan
#' 
#' @importFrom pbapply pblapply
#' @export

GroupSubsets <- function(groupnames){
    tmp <- setNames(pblapply(groupnames,function(x){
                 return(list("alku"=list(
                    "fi"=d[d$lang=="fi"&d$group==x&d$location3=="S1",],
                    "ru"=d[d$lang=="ru"&d$group==x&d$location3=="S1",]),
                 "keski"=list(
                    "fi"=d[d$lang=="fi"&d$group==x&d$location3=="S2/S3",],
                    "ru"=d[d$lang=="ru"&d$group==x&d$location3=="S2/S3",]),
                 "loppu"=list(
                    "fi"=d[d$lang=="fi"&d$group==x&d$location3=="S4",],
                    "ru"=d[d$lang=="ru"&d$group==x&d$location3=="S4",])
                    ))}),groupnames)
    return(tmp)
}
