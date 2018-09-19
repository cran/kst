### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### as.relation.kfamset
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(relations), library(sets)
###
### 2018-09-17: created
###

as.relation.kfamset <- function(x,...) {

   ### check x
   if (!inherits(x, "kfamset")) {
      stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kfamset")))
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
