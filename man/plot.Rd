\name{plot}
\alias{plot.kstructure}
\title{Plot Knowledge Structures}
\description{
  Plots a Hasse Diagram of knowledge structures.
}
\usage{
  \method{plot}{kstructure}(x, ...)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}}.}
  \item{\dots}{Other arguments to be passed to methods.}
}
\seealso{
  \code{\link{kstructure}}
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\examples{
kst <- kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))
plot(kst)
}
\keyword{math}