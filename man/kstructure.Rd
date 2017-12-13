\name{kstructure}
\alias{kstructure}
\title{Knowledge Structure}
\description{
  Creates a knowledge structure from a surmise relation or knowledge states. 
}
\usage{
  kstructure(x)
}
\arguments{
  \item{x}{Either an endorelation (see \code{\link[relations]{relation}})
  representing a surmise relation, or a \code{\link[sets]{set}} of sets 
  each representing one knowledge state.}
}
\details{
  The most basic assumption of knowledge space theory is that every
  knowledge domain can be represented in terms of a set of domain problems
  \emph{Q}. Moreover, knowledge space theory assumes dependencies between
  these domain problems in that knowledge of a given domain problem or a
  subset of problems may be a prerequisite for knowledge of another, more
  difficult or complex domain problem. These prerequisite relations are
  realized by \emph{surmise relations}, which create a quasi-order between
  different domain problems. One advantage of these surmise relations is
  that they reduce the quantity of all possible solution patterns to a more
  manageable amount of \emph{knowledge states}. Each of these knowledge
  states represents the subset of domain problems an individual is capable
  of solving. The collection of all knowledge states captures the
  organization of the domain and is referred to as \emph{knowledge
  structure}.

  \code{kstructure} takes an endorelation representing a surmise relation
  or a set of sets each representing one knowledge state (e.g., one clause
  of a surmise system) and returns the corresponding knowledge structure.
  A knowledge structure always contains the empty set and \emph{Q}.
}
\note{
  Note that by default the quotes indicate the fact that the items 
  are represented by characters. For displaying purposes, these 
  quotes may be turned off by setting respective set options (see 
  \code{\link[sets]{options}}).
}
\value{
  An \R object of class \code{kstructure}.
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\seealso{
  \code{\link[relations]{relation}}, \code{\link[sets]{set}}, \code{\link[sets]{options}}
}
\examples{
# An endorelation representing a surmise relation
kst <- endorelation(graph=set(tuple(1,1), tuple(2,2), tuple(3,3),
  tuple(4,4), tuple(2,1), tuple(3,1), tuple(4,1),
  tuple(3,2), tuple(4,2)))
kstructure(kst)
# A set of sets representing knowledge states (e.g., clauses of a surmise system)
kst <- set(set("a"), set("a","b"), set("a","c"), set("d","e"), set("a","b","d","e"), 
   set("a","c","d","e"), set("a","b","c","d","e"))
kstructure(kst)
# Turning off the quotes for displaying purposes
sets_options("quote",FALSE)
kstructure(kst)
}
\keyword{math}
