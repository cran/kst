### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### as.relation.kstructure.R
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(relations), library(sets)
###
### 2008-05-29: created
### 2018-04-13: completely rewritten
###

as.relation.kstructure <- function(x,...) {

   ### check x
   if (!inherits(x, "kstructure")) {
      stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kstructure")))
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
