\name{kdomain}
\alias{kdomain}
\title{Domain of Knowledge Structures or Bases}
\description{
  Computes the domain of knowledge structures or bases.
}
\usage{
  kdomain(x)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}},
           \code{\link{kbase}}, or \code{\link{kfamset}}.}
}
\details{
  A \emph{domain} is a set of questions or items representing a field of
  knowledge.
}
\value{
  A \code{\link[sets]{set}} of items, each representing one question of the
  knowledge domain.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kstructure}}, \code{\link{kbase}},
  \code{\link{kfamset}}, \code{\link[sets]{set}}
}
\examples{
kst <- kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))
kdomain(kst)
}
\keyword{math}
