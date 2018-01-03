### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### closure.kbase.R
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(sets)
###
### 2017-12-13: created
###

closure.kbase <- function(x, operation=c("union", "intersection"), ...) {

   ### check x
   if (!inherits(x, "kbase")) {
      stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kbase")))
   }

   clos <- if(operation == "union") {
   ### compute closure under union
      dom <- kdomain(x)
      relmat <- t(sapply(x, function(z) dom %in% z))
      relmat <- binary_closure(relmat, operation)
      relmat <- relmat[order(rowSums(relmat)),]
      colnames(relmat) <- dom
      y <- as.set(apply(relmat,1,function(z)as.set(names(which(z)))))      
      y <- set_union(y, set(set(), dom))

   } else
      NextMethod()

   class(clos) <- c("kstructure", "set", "gset", "cset")
      
   ### return closure
   clos

}
