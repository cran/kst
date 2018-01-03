### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### kbase.R
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(sets)
###
### 2008-04-24: created
### 2017-12-13: Change return class
###

kbase <- function(x) {

   ### check x
   if (!inherits(x, "kstructure")) {
      stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kstructure")))
   }
   if (!kstructure_is_kspace(x)) {
      stop("'x' must be a knowledge space.")
   }

   ### compute base
   atoms <- katoms(x, kdomain(x))
   names(atoms) <- NULL
   base <- as.set(unlist(atoms, recursive=FALSE))
   class(base) <- c("kbase", class(base))

   ### return base
   base
}

