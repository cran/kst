\name{plot}
\alias{plot.kstructure}
\alias{plot.kbase}
\alias{plot.kfamset}
\title{Plot Family of Sets}
\description{
  Plots a Hasse diagram of a family of sets
}
\usage{
  \method{plot}{kstructure}(x, ...)
  \method{plot}{kbase}(x, ...)
  \method{plot}{kfamset}(x, ...)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}},
	  \code{\link{kbase}}, or \code{\link{kfamset}}.}
  \item{\dots}{Other arguments to be passed to methods.}
}
\details{
\code{plot} takes an arbitrary family of sets and plots a Hasse diagram. 
}
\seealso{
  \code{\link{kstructure}}, \code{\link{kbase}}, \code{\link{kfamset}}
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\examples{
fs <- kfamset(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))
if(require("Rgraphviz")) {plot(fs)}
}
\keyword{math}

