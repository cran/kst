\name{as.relation}
\alias{as.relation.kstructure}
\alias{as.relation.kbase}
\alias{as.relation.kfamset}
\title{Surmise Relations of Knowledge Structures}
\description{
  Computes the surmise relation of knowledge structures.
}
\usage{
  \method{as.relation}{kstructure}(x, \dots)
  \method{as.relation}{kbase}(x, \dots)
  \method{as.relation}{kfamset}(x, \dots)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}},
           \code{\link{kbase}}, or \code{\link{kfamset}}.}
  \item{\dots}{Currently not used.}
}
\details{
  \code{as.relation} takes an arbitrary knowledge structure and computes the
  surmise \code{\link[relations]{relation}} of the corresponding 
  quasi-ordinal knowledge space. Antisymmetric (and
  transitive) surmise relations may then be plotted as a Hasse diagram.
}
\value{
  An \R object of class \code{\link[relations]{relation}}.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kstructure}}, \code{\link{kbase}}, \code{\link{kfamset}},
  \code{\link[relations]{relation}}, \code{\link[relations]{plot}}
}
\examples{
kst <- kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))
as.relation(kst)
}
\keyword{math}
