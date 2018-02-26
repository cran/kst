\name{space_property}
\alias{kstructure_is_kspace}
\alias{kspace}
\title{Space Property of a Knowledge Structure}
\description{
  Tests for and converts to knowledge space.
}
\usage{
  kstructure_is_kspace(x)
  kspace(x)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}} or (for 
           \code{kspace()} only) \code{\link{kbase}}.}
}
\details{
  A knowledge structure is considered a knowledge space if it includes one
  state for the empty set \emph{\{\}}, one state for the full set of domain
  problems \emph{Q}, and a state for the union of any two knowledge states
  (i.e., the closure under union).

  \code{kstructure_is_kspace} takes an arbitrary knowledge structure and
   tests for its space property.

  \code{kspace} takes an arbitrary knowledge structure or base and 
  returns the corresponding knowledge space.
}
\value{
  For \code{kstructure_is_kspace} a logical value.

  For \code{kspace} an \R object of class \code{kspace} where each
  subset represents one knowledge state of the knowledge space.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kstructure}}, \code{\link{closure.kstructure}}
}
\examples{
kst <- kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))

# test for knowledge space
kstructure_is_kspace(kst)

# convert to knowledge space
kspace(kst)
}
\keyword{math}
