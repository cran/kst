### kneighbourhood.R
###
### Compute the neighbourhood of a knowledge state
###
kneighbourhood <- function(kst, state) {
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
    if (set_cardinality(d) == 1) {
      n <- set_union(n, set(k))
    }
  }
  class(n) <- unique(c("kfamset", class(n)))
  return(n)
}

