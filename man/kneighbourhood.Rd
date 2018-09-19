\name{kneighbourhood}
\alias{kneighbourhood}
\title{Neighbourhood of Knowledge States}
\description{
  Computes the neighbourhood of a knowledge state.
}
\usage{
  kneighbourhood(kst, state)
}
\arguments{
  \item{kst}{An \R object of class \code{\link{kstructure}}.}
  \item{state}{An \R object of class \code{\link[sets]{set}}.}
}
\details{
  The neighbourhood of a knowledge state is the set of all those states which
  have a symmetric seu difference of 1.
}
\value{
  A set of sets containing the neighbourhood  of \code{state}
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kfringe}}, \code{\link{kstructure}}, \code{\link[sets]{set}}
}
\examples{
kst <- kstructure(set(set(), set("c"), set("a","b"), set("b","c"), 
   set("c","d"), set("d","e"), set("a","b","c"), set("b","c","d"), 
   set("c","d","e"), set("a","b","c","d"), set("a","b","d","e"), 
   set("b","c","d","e"), set("a","b","c","d","e")))

# inner fringe
kneighbourhood(kst, set("c","d","e"))
}
\keyword{math}
