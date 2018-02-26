\name{lpath}
\alias{lpath}
\title{Learning Paths in a Knowledge Structure}
\description{
  Computes learning paths in a knowledge structure.
}
\usage{
  lpath(x)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}}.}
}
\details{
  A learning path in a knowledge structure is a maximal sequence of 
  knowledge states, which allows learners to gradually traverse a 
  knowledge structure from the empty set \emph{\{\}} (or any other 
  bottom state) to the full set of domain problems \emph{Q}. 
  Mathematically, it is represented as a set of states.

  \code{lpath} takes an arbitrary knowledge structure and computes all 
  possible learning paths in the respective knowledge structure.
}
\value{
  A list where each element represents one learing path.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kstructure}}
}
\examples{
kst <- kstructure(set(set(), set("a"), set("b"), set("a","b"), 
   set("a","d"), set("b","c"), set("a","b","c"), set("a","b","d"), 
   set("b","c","d"), set("a","b","c","d"), set("a","b","c","d","e")))
lpath(kst)
}
\keyword{math}
