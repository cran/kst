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
   atoms <- katoms(x, items=dom)
   relmat <- mat.or.vec(length(dom), length(dom))
   colnames(relmat) <- dom
   rownames(relmat) <- dom
   for (i in 1:length(dom)) {
      items <- unique(unlist(atoms[[i]]))
      relmat[items,i] <- 1
   }
   rel <- as.relation(relmat)

   ### return results
   rel
}
