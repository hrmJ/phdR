PhdR -- R functions, model descriptions and data related to my phd thesis
=========================================================================

This repository contains the R code used in my phD thesis. This can be
installed as an R package, and the easiest way to do that is to use the
`remotes`-packages (available from CRAN, install with
`install.packages('remotes')`):

```r

library(remotes)
github_install("hrmJ/phdR")

```


The package includes the *numerical* data used in the study (the data frame,
or, more precisely, tibble `d`), i.e. I've stripped all the sentences and
contexts in order to avoid possible copyright or privacy issues.

The bugs code for the bayesian models used in the thesis are available
in the folder inst/models. The python code that generated those 
models is available in [this repository](https://github.com/hrmJ/phdBayes).
