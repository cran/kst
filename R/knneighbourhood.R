### knneighbourhood.R
###
### Compute the n-neighbourhood of a knowledge state
###
knneighbourhood <- function(kst, state, distance) {
  ## Check parameters
  if (!inherits(kst, "kstructure"))
    stop(sprintf("%s must be of class %s.", dQuote("kst"), dQuote("kstructure")))
  if (!inherits(state, "set"))
    stop(sprintf("%s must be of class %s.", dQuote("state"), dQuote("set")))
  if (!set_contains_element(kst, state))
    stop(sprintf("Specified state is no element of %s", dQuote("x")))
  
  n <- set()
  for (k in kst) {
    d <- set_symdiff(k, state)
    dist <- set_cardinality(d)
    if ((dist >= 1) && (dist <= distance)) {
      n <- set_union(n, set(k))
    }
  }
  class(n) <- unique(c("kfamset", class(n)))
  return(n)
}

