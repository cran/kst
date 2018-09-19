### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### closure.kfamset
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(sets)
###
### 2017-12-13: created
###

closure.kfamset <- function(x, operation=c("union", "intersection"), ...) {

   ### check x
   if (!inherits(x, "kfamset")) {
      stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kfamset")))
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

   if (operation == "union")
      class(clos) <- c("kspace", "kstructure", "kfamset", "set", "gset", "cset")
   else
      class(clos) <- c("kstructure", "kfamset", "set", "gset", "cset")
      
   ### return closure
   clos

}
