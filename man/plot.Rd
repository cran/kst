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
\note{
   Note that the \code{plot} method makes use of the package 
   \code{Rgraphviz}. \code{Rgraphviz} v1.18 or higher may cause 
   problems on Windows operating systems. For this reason, 
   \code{Rgraphviz} v1.16.0 is provided in the \code{inst} 
   directory of this package and may be installed manually in case 
   of plotting errors.
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