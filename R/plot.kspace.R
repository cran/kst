### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### plot.kspace
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(relations), library(sets), library(Rgraphviz)
###
### 2008-04-17: created
###

plot.kspace <- function(x, ...) {

   ### check if package Rgraphviz is available
   if (!requireNamespace("Rgraphviz", quietly = TRUE)) {
     stop(sprintf("Plotting requires package 'Rgraphviz'."))
   }

   ### check x
   if (!inherits(x, "kspace")) {
      stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kspace")))
   }

   ### compute structure matrix
   relmat <- set_outer(x, set_is_subset)

   ### plot results
   Rgraphviz::plot(relation(incidence=relmat, domain=x), ...)

}
