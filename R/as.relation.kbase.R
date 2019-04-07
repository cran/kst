### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### as.relation.kbase.R
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(relations), library(sets)
###
### 2017-12-13: created
### 2018-04-13: completely rewritten
### 2019-04-07: Replacing nested for-loops with lapply commands
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
