### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### as.famset.R
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(sets)
###
### 2018-03-01 created
###

as.famset <- function(m, as.letters = TRUE) {
  if (!inherits(m, "matrix")) {
    stop(sprintf("%s must be a binary matrix.", dQuote("m")))
  }
  if (sum(!(m == 0 | m == 1))) {
    stop(sprintf("%s must be a binary matrix.", dQuote("m")))
  }
  if (as.letters) {
    names <- make.unique(letters[0:(ncol(m)-1) %% 26 + 1])
  } else {
    names <- as.integer(1L:ncol(m))
  }
  fam <- set()
  apply(m, 1, function(v) {
    fam <<- set_union(fam, set(as.set(names[which(v==1)])))
  })
  
  fam
}
