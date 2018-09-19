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
###

kspace <- function(x) {

   ### check x
   if (!inherits(x, "kstructure") & !inherits(x, "kbase")) {
      stop(sprintf("%s must be of class %s or %s.", 
        dQuote("x"), 
	dQuote("kstructure"),
	dQuote("kbase")
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
