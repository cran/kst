\name{lpath_is_gradation}
\alias{lpath_is_gradation}
\title{Gradation Property of Learning Paths}
\description{
  Tests for the gradation property of learning paths.
}
\usage{
  lpath_is_gradation(x)
}
\arguments{
  \item{x}{A \code{list} of learning paths .}
}
\details{
  A learning path is considered a \emph{gradation} if each state in a 
  learning path differs from its predecessor and/or successor state by 
  a single item/notion.

  \code{lpath_is_gradation} takes an arbitrary list of learning paths and 
  tests for their gradation property.
}
\value{
  A \code{list} of logical values where each element represents one learning path.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kstructure}}, \code{\link{lpath}}
}
\examples{
kst <- kstructure(set(set(), set("c"), set("a","b"), set("b","c"), 
   set("c","d"), set("d","e"), set("a","b","c"), set("b","c","d"), 
   set("c","d","e"), set("a","b","c","d"), set("a","b","d","e"), 
   set("b","c","d","e"), set("a","b","c","d","e")))
lp <- lpath(kst)
lpath_is_gradation(lp)
}
\keyword{math}