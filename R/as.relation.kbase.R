### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### as.relation.kbase.R
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(relations), library(sets)
###
### 2017-12-13: created
###

as.relation.kbase <- function(x,...) {

   ### check x
   if (!inherits(x, "kbase")) {
      stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kbase")))
   }

   ### compute relation
   dom <- kdomain(x)
   relmat <- mat.or.vec(length(dom), length(dom))
   colnames(relmat) <- dom
   rownames(relmat) <- dom
   strmat <- as.binaryMatrix(x)
   for (i in 1:dim(strmat)[2]) {
     for (j in 1:dim(strmat)[2]) {
       if (all(strmat[,i] <= strmat[,j])) {
         relmat[j,i] <- 1
       }
     }
   }
   rel <- as.relation(relmat)
   rel
}
