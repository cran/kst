### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### as.relation.kspace
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(relations), library(sets)
###
### 2022-07-13: created
###

as.relation.kspace <- function(x,...) {

   ### check x
   if (!inherits(x, "kspace")) {
      stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kspace")))
   }

   ### compute relation
   dom <- kdomain(x)
   relmat <- mat.or.vec(length(dom), length(dom))
   colnames(relmat) <- dom
   rownames(relmat) <- dom
   strmat <- as.binaryMatrix(x)
   lapply(as.list(1:dim(strmat)[2]), function(i) {
     lapply(as.list(1:dim(strmat)[2]), function(j) {
       if (all(strmat[,i] <= strmat[,j])) {
         relmat[j,i] <<- 1
       }
     })
   })
   storage.mode(relmat) <- "integer"
   rel <- as.relation(relmat)
   rel
}
