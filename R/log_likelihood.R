


#' R function for calculating LL, by Andrew Hardie, Lancaster University.
#' 
#' http://corpora.lancs.ac.uk/sigtest/process.php?action=seeR
#' 
#'  CHeckout rfast package, too (requires libgls-dev)
#' 
#' 
#' @param O matrix in the format 2 X 2 e.g. matrix(c(52,50000,57,75000),byrow=F,ncol=2)
#' @export

loglikelihood.test = function(O)
{
    DNAME <- deparse(substitute(O))

    E = suppressWarnings(chisq.test(O)$expected)

    sum = 0;

    for(i in 1:length(O[,1]))
    {
        for(j in 1:length(O[1,]))
        {
            if (O[i,j] == 0 || E[i,j] == 0)
                next
            sum = sum + (O[i,j] * log(O[i,j]/E[i,j]))
        }
    }
    STAT = sum * 2;

    DF = (length(O[1,]) - 1) * (length(O[,1]) - 1)

    P = 1 - pchisq(STAT, df=DF)

    names(DF) = "df"
    names(STAT) = "Log-likelihood"
    
    obj =  list(statistic=STAT, parameter=DF, p.value=P, method="Log-Likelihood test", 
                data.name=DNAME, observed=O, expected=E)

    attr(obj, "class") <- "htest"

    return (obj)
}
