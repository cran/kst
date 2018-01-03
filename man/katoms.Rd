\name{katoms}
\alias{katoms}
\title{Atoms of Knowledge Structures}
\description{
  Computes atoms of knowledge structures.
}
\usage{
  katoms(x, items)
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}} or 
           \code{\link{kbase}}.}
  \item{items}{A \code{\link[sets]{set}} of items for which atoms are computed.}
}
\details{
  For any item \emph{q} of the knowledge domain \emph{Q}, an \emph{atom at q}
  is a minimal knowledge state containing \emph{q}, where minimal refers to
  the fact that the respective knowledge state is not the union of any
  other knowledge states.
}
\value{
  A list where each element represents the atom(s) of one item specified in
  \code{items}.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link{kstructure}}, \code{\link[sets]{set}}
}
\examples{
kst <- kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))
katoms(kst, items=set("a","b","c"))
}
\keyword{math}
