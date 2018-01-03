### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### kdomain.R
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(sets)
###
### 2008-06-18: created
### 2017-12-13: Allowing kbase parameter
###

kdomain <- function(x) {

   ### check x
   if (!inherits(x, "kstructure") & !inherits(x, "kbase")) {
      stop(sprintf("%s must be of class %s or %s.", 
        dQuote("x"), 
	dQuote("kstructure"),
	dQuote("kbase")
      ))
   }

   ### compute domain
   domain <- as.set(unique(unlist(as.list(x))))

   ### return domain
   domain
}
