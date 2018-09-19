### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### plot.kbase
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(relations), library(sets), library(Rgraphviz)
###
### 2018-09-17: created
###

plot.kbase <- function(x, ...) {

   ### check if package Rgraphviz is available
   if (!requireNamespace("Rgraphviz", quietly = TRUE)) {
     stop(sprintf("Plotting requires package 'Rgraphviz'."))
   }

   ### check x
   if (!inherits(x, "kbase")) {
      stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kbase")))
   }

   ### compute structure matrix
   relmat <- set_outer(x, set_is_subset)

   ### plot results
   Rgraphviz::plot(relation(incidence=relmat, domain=x), ...)

}
