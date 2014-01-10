\name{as.relation}
\alias{as.relation.kstructure}
\title{Surmise Relations of Knowledge Structures}
\description{
  Computes the surmise relation of knowledge structures.
}
\usage{
  \method{as.relation}{kstructure}(x, \dots)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}}.}
  \item{\dots}{Currently not used.}
}
\details{
  \code{as.relation} takes an arbitrary knowledge structure and computes its
  underlying surmise \code{\link[relations]{relation}}, i.e., the set of
  item paris corresponding to the knowledge dependencies. Antisymmetric and
  transitive surmise relations may then be plotted as a Hasse diagram.
}
\value{
  An \R object of class \code{\link[relations]{relation}}.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kstructure}}, \code{\link[relations]{relation}}, \code{\link[relations]{plot.relation}}
}
\examples{
kst <- kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))
as.relation(kst)
}
\keyword{math}