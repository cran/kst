### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### kspace.R
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(sets)
###
### 2008-04-17: created
### 2017-12-13: Allowing kbase parameter, setting result class explicitly
### 2026-03-08: Generalizing to allowing kfamset parameter
###

kspace <- function(x) {
  
  ### check x
  if (!inherits(x, "kfamset") & !inherits(x, "kbase")) {
    stop(sprintf("%s must be of class %s.", 
                 dQuote("x"), 
                 dQuote("kfamset")
    ))
  }
  
  ### compute knowledge space
  dom <- kdomain(x)
  space <- c(x, set(dom), set(set()))
  class(space) <- class(x)
  space <- closure(space, operation="union")
  class(space) <- c("kspace", "kstructure", "kfamset", "set", "gset", "cset")
  
  ### return space
  space
}
