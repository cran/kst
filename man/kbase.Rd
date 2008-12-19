\name{kbase}
\alias{kbase}
\title{Base of a Knowledge Space}
\description{
  Computes the base of a knowledge space.
}
\usage{
  kbase(x)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kspace}}.}
}
\details{
  A \emph{base} for a knowledge space is a minimal family of knowledge
  states spanning the knowledge space, i.e., the base includes the minimal
  states sufficient to reconstruct the full knowledge space. A knowledge
  structure has a base only if it is a knowledge space.
}
\value{
  A \code{\link[sets]{set}} of sets where each subset represents one knowledge
  state of the base.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kspace}}, \code{\link{kstructure}}, \code{\link[sets]{set}}
}
\examples{
kst <- kspace(kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e"))))
kbase(kst)
}
\keyword{math}