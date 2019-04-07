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
### 2018-09-19: Allowing kfamset parameter
### 2019-04-07: Bugfix of kfamset parameter
###

kdomain <- function(x) {

   ### check x
   if (!inherits(x, "kstructure") & !inherits(x, "kbase")
       & !inherits(x, "kfamset")) {
      stop(sprintf("%s must be of class %s, %s, or %s.", 
        dQuote("x"), 
	dQuote("kstructure"),
	dQuote("kbase"),
	dQuote("kfamset")
      ))
   }

   ### compute domain
   domain <- as.set(unique(unlist(as.list(x))))

   ### return domain
   domain
}
