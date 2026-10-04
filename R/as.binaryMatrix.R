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
  if (!inherits(x, "kfamset")) {
    stop(sprintf("%s must be of class %s.", dQuote("x"), dQuote("kfamset")))
  }

  states <- lapply(x, as.character)
  items <- sort(unique(unlist(states)))
  R <- matrix(0, length(x), length(items),
              dimnames=list(NULL, items))
  for (i in seq_len(nrow(R))) R[i, states[[i]]] <- 1
  storage.mode(R) <- "integer"

    if (inherits(x, "kspace")) class(R) <- unique(c("kmspace", "kmstructure", "kmfamset", class(R)))
  else if (inherits(x, "kstructure")) class(R) <- unique(c("kmstructure", "kmfamset", class(R)))
  else if (inherits(x, "kbasis")) class(R) <- unique(c("kmbasis", "kmfamset", class(R)))
  else class(R) <- unique(c("kmfamset", class(R)))
  
  R
}
