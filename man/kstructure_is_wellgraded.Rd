\name{kstructure_is_wellgraded}
\alias{kstructure_is_wellgraded}
\title{Well-Gradedness of Knowledge Structures}
\description{
  Tests for the well-gradedness of knowledge structures.
}
\usage{
  kstructure_is_wellgraded(x)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}}.}
}
\details{
  A knowledge structure is considered \emph{well-graded} if any two of 
  its states are connected by a bounded path, i.e., each knowledge state 
  (except the state for the full set of domain problems \emph{Q}) has at 
  least one immediate successor state that comprises the same domain items 
  plus exactly one and each knowledge state (except the empty set \emph{\{\}}) 
  has at least one predecessor state that contains the same domain items 
  with the exception of exactly one. 

  \code{kstructure_is_wellgraded} takes an arbitrary knowledge structure 
  and tests for its well-gradedness.
}
\value{
  A logical value.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kstructure}}
}
\examples{
kst <- kstructure(set(set(), set("a"), set("b"), set("c"), set("a","b"), 
   set("b","c"), set("a","b","c")))
kstructure_is_wellgraded(kst)

kst <- kstructure(set(set(), set("a"), set("b"), set("c"), set("a","b"), 
   set("a","b","c")))
kstructure_is_wellgraded(kst)
}
\keyword{math}