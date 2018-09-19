\name{reduction}
\alias{reduction.kstructure}
\alias{reduction.kfamset}
\title{Reduction of Knowledge Structures}
\description{
  Computes the reduction of knowledge structures.
}
\usage{
  \method{reduction}{kstructure}(x, operation=c("discrimination", "union", "intersection"),...)
  \method{reduction}{kfamset}(x, operation=c("discrimination", "union", "intersection"),...)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}} or \code{\link{kfamset}}.}
  \item{operation}{The set operation under which the reduction is computed.}
  \item{\dots}{Other arguments to be passed to methods.}
}
\details{
  \code{reduction} performs the reduction of a knowledge structure by
  computing the minimal subset having the same closure as the knowledge
  structure. Additionally, it allows for computing the \emph{discriminative}
  reduction of a knowledge structure. Such a discriminative reduction is a
  knowledge structure in which each notion contains a single item.
}
\value{
  An \R object of the same class as \code{x} where each subset represents
  one knowledge state of the resulting reduction.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kstructure}}, \code{\link{knotions}}, \code{\link[sets]{closure}}
}
\examples{
kst <- kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))
reduction(kst, operation="discrimination")
}
\keyword{math}
