
#' S4 class for easy representation of tables of Lambrechtian pragmatic assertions and presuppositions. This class represents sentences
#' 
#' @export
#' 

setClass("PragmaticAssertionSentence", slots = c(sentence="character", presuppositions = "vector", assertions = "vector") )

#' S4 class for easy representation of tables of Lambrechtian pragmatic assertions and presuppositions. This class represents the actual tables
#' 
#' @export
#' 

setClass("PragmaticAssertionTab", slots = c(sentences="list",matr="matrix",caption="character"))

setGeneric(name="PrintTab", def=function(object) { standardGeneric("PrintTab") })

#' Prints out a table representing pragmatic assertions/presuppositions
#' 
#' @importFrom kableExtra kable kable_styling column_spec
#' @importFrom dplyr  %>% 
#' @export
#' 
setMethod("PrintTab", "PragmaticAssertionTab",
            function(object) {
                m.data <- c()
                m.rownames <- c()
                for(sentence in object@sentences){
                    m.data <- c(m.data,
                                    paste(sentence@presuppositions,collapse=". "),
                                    paste(sentence@assertions,collapse=". "))
                    m.rownames <- c(m.rownames,sentence@sentence)
                }
                matr <-as.data.frame(matrix(m.data, byrow=T,ncol=2))
                matr.out  <- data.frame("lause"=m.rownames,"olettama"=matr[,1],"väittämä"=matr[,2])


                matr.out %>%
                    kable(caption=object@caption,  booktabs=T) %>% 
                    kable_styling (latex_options = c("striped","HOLD_position"),full_width = T) 
                    #column_spec(1, italic=T)
            }
)

