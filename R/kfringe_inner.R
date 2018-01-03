### kfringe_inner.R
###
### Compute the inner fringe of a knowledge state
###

kfringe_inner <- function(kst, state) {
  ## Check parameters
  if (!inherits(kst, "kstructure"))
    stop(sprintf("%s must be of class %s.", dQuote("kst"), dQuote("kstructure")))
  if (is.null(state)) {
    f <- list()
    i <- 1
    for (s in kst) {
      x <- kfringe_inner(kst, s)
      f[[i]] <- x
      i <- i + 1
    }
    return(f)
  }
  if (!inherits(state, "set"))
    stop(sprintf("%s must be of class %s.", dQuote("state"), dQuote("set")))
  if (!set_contains_element(kst, state))
    stop(sprintf("Specified state is no element of %s", dQuote("x")))

  n <- kneighbourhood(kst, state)
  
  f <- set()
  for (i in n) {
    if (set_is_subset(i, state))
      f <- set_union(f, set_symdiff(i, state))
  }
  return(f)

}
