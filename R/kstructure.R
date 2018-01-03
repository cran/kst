### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### kstructure.R
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(sets), library(relations)
###
### 2008-05-02: created
### 2017-12-11: Include empty set and domain into result
###

kstructure <- function(x) {

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
   dom <- as.set(unique(unlist(as.list(x))))
   x <- set_union(x, set(set(), dom))
   names(x) <- NULL
   class(x) <- unique(c("kstructure", class(x)))

   ### return structure
   x
}
