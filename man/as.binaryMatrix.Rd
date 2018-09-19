\name{as.binaryMatrix}
\alias{as.binaryMatrix}
\title{Matrix Representation of Knowledge Structures}
\description{
  Computes the matrix representation of a knowledge structure.
}
\usage{
  as.binaryMatrix(x)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}} (or
           \code{\link{kspace}}).}
}
\details{
  \code{as.binaryMatrix} takes an arbitrary knowledge structure in set
  representation and computes its matrix form.
}
\value{
  An \R object of class \code{matrix}.
}
\seealso{
  \code{\link{as.famset}}, \code{\link{kspace}}, \code{\link{kstructure}}
}
\examples{
kst <- kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))
as.binaryMatrix(kst)
}
\keyword{math}
