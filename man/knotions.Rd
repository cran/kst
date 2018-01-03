\name{knotions}
\alias{knotions}
\title{Notions of Knowledge Structures or Bases}
\description{
  Computes notions of knowledge structures or bases.
}
\usage{
  knotions(x)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}} or
           \code{\link{kbase}}.}
}
\details{
  A \emph{notion} is a set of items always jointly contained in some
  knowledge states. Consequently, these items carry the same information
  and may therefore be considered equivalent. A knowledge structure where
  each notion contains only one item is considered discriminative.
}
\value{
  A \code{\link[sets]{set}} of sets, each representing one notion of the
  knowledge structure.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{reduction.kstructure}}, \code{\link{kstructure}}, \code{\link[sets]{set}}
}
\examples{
kst <- kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))
knotions(kst)
}
\keyword{math}
