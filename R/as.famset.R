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
  if (!inherits(m, "kmfamset")) {
    stop(sprintf("'m' must be a 'kmfamset' object."))
  }
  if (sum(!(m == 0 | m == 1))) {
    stop(sprintf("'m' must be a binary matrix."))
  }
  if (!is.null(colnames(m))) {
    names <- colnames(m)
  } else if (as.letters) {
    names <- make.unique(letters[(0L:(ncol(m)-1)) %% 26 + 1])
  } else {
    names <- as.integer(1L:ncol(m))
  }
  fam <- set()
  apply(m, 1, function(v) {
    fam <<- set_union(fam, set(as.set(names[which(v==1)])))
  })

  if (inherits(m, "kmspace")) class(fam) <- unique(c("kspace", "kstructure", "kfamset", class(fam)))
  else if (inherits(m, "kmstructure")) class(fam) <- unique(c("kstructure", "kfamset", class(fam)))
  else if (inherits(m, "kmbasis")) class(fam) <- unique(c("kbasis", "kfamset", class(fam)))
  else class(fam) <- unique(c("kfamset", class(fam)))
  
  fam
}
