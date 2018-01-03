\name{kfringe}
\alias{kfringe}
\alias{kfringe_inner}
\alias{kfringe_outer}
\title{Fringes of Knowledge States}
\description{
  Computes the fringe of a knowledge state.
}
\usage{
  kfringe(kst, state)

  kfringe_inner(kst, state)

  kfringe_outer(kst, state)
}
\arguments{
  \item{kst}{An \R object of class \code{\link{kstructure}}.}
  \item{state}{An \R object of class \code{\link[sets]{set}}.}
}
\details{
  The fringe determines the symmetric difference between a given knowledge 
  state and its neighbouring states. It is divided into inner and outer fringe.
  The inner fringe contains the fringe items which are element of the knowledge 
  state. They have probably been recently learned. The outer fringe contains those 
  fringe items which are noe element of the knowledge state. For these items,
  all prerequisites are fulfilled, i.e. the learner is ready to learn them now.
}
\value{
  A set contining the fringe of \code{state}. If \code{state} is \code{NULL} then a
  list containing the fringes of all knowledge states is returned.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kneighbourhood}}, \code{\link{kstructure}}, \code{\link[sets]{set}}
}
\examples{
kst <- kstructure(set(set(), set("c"), set("a","b"), set("b","c"), 
   set("c","d"), set("d","e"), set("a","b","c"), set("b","c","d"), 
   set("c","d","e"), set("a","b","c","d"), set("a","b","d","e"), 
   set("b","c","d","e"), set("a","b","c","d","e")))

# fringe
kfringe(kst, set("c","d","e"))
}
\keyword{math}
