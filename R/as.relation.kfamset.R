### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### as.relation.kfamset
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(relations), library(sets)
###
### 2018-09-17: created
### 2019-04-07: Replacing nested for-loops with lapply commands
###

as.relation.kfamset <- function(x,...) {
  
   ### check x
   if (!inherits(x, "kfamset")) {
      stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kfamset")))
   }

   ### compute relation
   dom <- kdomain(x)
   relmat <- mat.or.vec(length(dom), length(dom))
   rml <- list(m = relmat)
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
