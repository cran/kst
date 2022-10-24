\name{knneighbourhood}
\alias{knneighbourhood}
\title{Neighbourhood of Knowledge States}
\description{
  Computes the neighbourhood of a knowledge state.
}
\usage{
  knneighbourhood(kst, state, distance)
}
\arguments{
  \item{kst}{An \R object of class \code{\link{kstructure}}.}
  \item{state}{An \R object of class \code{\link[sets]{set}}.}
  \item{distance}{An integer specifying the size of the neighbourhood}
}
\details{
  The n-neighbourhood of a knowledge state is the set of all those states which
  have a symmetric seu difference of not more than n.
}
\value{
  A set of sets containing the n-neighbourhood  of \code{state}
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kneighbourhood}}, \code{\link{kfringe}},
  \code{\link{kstructure}}, \code{\link[sets]{set}}
}
\examples{
kst <- kstructure(set(set(), set("c"), set("a","b"), set("b","c"), 
   set("c","d"), set("d","e"), set("a","b","c"), set("b","c","d"), 
   set("c","d","e"), set("a","b","c","d"), set("a","b","d","e"), 
   set("b","c","d","e"), set("a","b","c","d","e")))

knneighbourhood(kst, set("c","d","e"), 2)
}
\keyword{math}
