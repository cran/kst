### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### as.matrix.kstructure.R
###
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
###
### dependencies: library(sets)
###
### 2018-04-13: created
###


as.binaryMatrix <- function(x) {

   ### check x
  if (!inherits(x, "set")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("set")))
  }

  states <- lapply(x, as.character)
  items <- sort(unique(unlist(states)))
  R <- matrix(0, length(x), length(items),
              dimnames=list(NULL, items))
  for (i in seq_len(nrow(R))) R[i, states[[i]]] <- 1
  storage.mode(R) <- "integer"
  R
}
