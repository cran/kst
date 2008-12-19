\name{ktrace}
\alias{ktrace}
\title{Trace of Knowledge Structures}
\description{
  Computes the trace of knowledge structures.
}
\usage{
  ktrace(x, items)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}}.}
  \item{items}{A set of items for which the trace is computed.}
}
\details{
  The \emph{trace} of a knowledge structure \emph{K} on a set \emph{A} is the
  substructure of the knowledge structure \emph{K} on the set \emph{A}, i.e.,
  the substructure resulting from restricting the knowledge structure
  \emph{K} to the items specified in \emph{A}.
}
\value{
  An \R object of class \code{\link{kstructure}} where each element
  represents one knowledge state of the knowledge structure on the item
  specified in \code{items}.
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
ktrace(kst, items=set("c","d","e"))
}
\keyword{math}