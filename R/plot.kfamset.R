### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### plot.kfamset
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(relations), library(sets), library(Rgraphviz)
###
### 2018-09-17: created
###

plot.kfamset <- function(x, ...) {

   ### check if package Rgraphviz is available
   if (!requireNamespace("Rgraphviz", quietly = TRUE)) {
     stop(sprintf("Plotting requires package 'Rgraphviz'."))
   }

   ### check x
   if (!inherits(x, "kfamset")) {
      stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kfamset")))
   }

   ### compute structure matrix
   relmat <- set_outer(x, set_is_subset)

   ### plot results
   Rgraphviz::plot(relation(incidence=relmat, domain=x), ...)

}
