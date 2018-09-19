### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### kfamset
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(sets), library(relations)
###
### 2018-09-17: created
###

kfamset <- function(x) {

   ### check x
   if (!inherits(x, "relation") & !inherits(x, "set")) {
      stop(sprintf("%s must be a relation or a set of subsets.",dQuote("x")))
   }

   ### convert relations to sets
   if (inherits(x, "relation")) {
      relmat <- relation_incidence(x)
      mode(relmat) <- "logical"
      x <- as.set(apply(relmat,2,function(z)as.set(names(which(z)))))      
   } else {
      x <- as.set(lapply(lapply(x, as.character),as.set))
   }
   names(x) <- NULL
   class(x) <- unique(c("kfamset", class(x)))

   ### return structure
   x
}
