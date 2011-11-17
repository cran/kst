\name{fringe}
\alias{kfringe}
\alias{kfringe_inner}
\alias{kfringe_outer}
\title{Fringes of Knowledge States}
\description{
  Computes the inner and outer fringe of knowledge states.
}
\usage{
  kfringe(x, state=NULL, operation=c("inner", "outer"))
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}}.}
  \item{state}{\code{NULL} (computes the fringes for all knowledge 
  states in \code{x}) or an \R object of class \code{\link[sets]{set}} 
  (computes the fringe of a single knowledge state).}
  \item{operation}{Either \code{"inner"} or \code{"outer"}.}
}
\details{
  Fringes determine the symmetric difference between a given knowledge 
  state and its neighbouring states.

  The symmetric difference between a given knowledge state and its 
  predecessor states is referred to as \emph{inner fringe}. These items 
  can be interpreted as most recently learned and may for example be 
  subject to learner review or assessment.

  The symmetric difference between a given knowledge state and its 
  immediate successor states is referred to as \emph{outer fringe}. 
  These items may be considered those a learner should tackle next and 
  therefore allow for creating personalized learning paths.
}
\value{
  A list where each element represents the inner or outer fringe of 
  one knowledge state respectively.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kstructure}}, \code{\link[sets]{set}}
}
\examples{
kst <- kstructure(set(set(), set("c"), set("a","b"), set("b","c"), 
   set("c","d"), set("d","e"), set("a","b","c"), set("b","c","d"), 
   set("c","d","e"), set("a","b","c","d"), set("a","b","d","e"), 
   set("b","c","d","e"), set("a","b","c","d","e")))

# inner fringe
kfringe(kst, operation="inner")
kfringe(kst, state=set("b", "c", "d", "e"), operation="inner")

# outer fringe
kfringe(kst, operation="outer")
kfringe(kst, state=set("b", "c", "d", "e"), operation="outer")
}
\keyword{math}