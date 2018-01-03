### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### knotions.R
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(sets)
###
### 2008-04-24: created
### 2017-12-13: Allowing kbase parameter
###

knotions <- function(x) {

   ### check x
   if (!inherits(x, "kstructure") & !inherits(x, "kbase")) {
      stop(sprintf("%s must be of class %s or %s.", 
        dQuote("x"), 
	dQuote("kstructure"),
	dQuote("kbase")
      ))
   }

   ### calculate notions
   dom <- kdomain(x)
   kstates <- sapply(x, function(z) dom %in% z)
   rownames(kstates) <- dom
   notions <- set()
   for (i in seq_len(nrow(kstates))) {
      n <- which(apply(kstates,1,function(z)all(z==kstates[i,])))
      notions <- c(notions, set(as.set(names(n))))
   }

   ### return notions
   notions
}
