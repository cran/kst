\name{kassess}
\alias{kassess}
\title{Assess Individuals}
\description{
  Assigns individuals to their corresponding knowledge states.
}
\usage{
  kassess(x, rpatterns=NULL, method="deterministic")
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}}.}
  \item{rpatterns}{A binary data frame or matrix where each row specifies
  the response pattern of one individual to the set of domain problems in \code{x}.}
  \item{method}{The desired assessment method. Currently only
  \code{"deterministic"} assessment is implemented.}
}
\details{
  \code{kassess} assigns individuals to their corresponding knowledge state
  in a knowledge structure.

  Assessing individuals based on a \code{"deterministic"} procedure
  starts by determining a domain problem \emph{a}, which is contained in
  approximately half of the available knowledge states. If the individual
  being assessed has successfully solved the respective domain problem \emph{a},
  all knowledge states that do not contain domain problem \emph{a} are
  removed from the set of potential knowledge states of the individual. If,
  on the other hand, the individual has not solved the respective domain
  problem \emph{a}, all knowledge states that do contain domain problem \emph{a}
  are removed from the set of potential knowledge states of the individual.
  From the remaining knowledge states a domain problem \emph{b}, which again
  is contained in approximately half of the still available knowledge states,
  is selected. If the individual has successfully solved the respective
  domain problem \emph{b}, all knowledge states that do not contain domain
  problem \emph{b} are removed from the set of potential knowledge states
  of the individual. If, on the other hand, the individual has solved the
  respective domain problem \emph{b}, all knowledge states that do contain
  domain problem \emph{b} are removed from the set of potential knowledge
  states of the individual. This procedure is repeated until only one
  knowledge state is left. This is the knowledge state the individual is
  currently located in.
}
\value{
  A list where each element represents the knowledge state of one respondent.
}
\seealso{
  \code{\link{kstructure}}
}
\references{
  Doignon, J.-P., Falmagne, J.-C. (1999) \emph{Knowledge Spaces}. Heidelberg:
  Springer Verlag.
}
\examples{
# deterministic assessment
kst <- kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))
rp <- data.frame(a=c(1,1,0,1,1,1,1,0,0,0),b=c(0,1,0,1,0,1,0,1,0,0),
   c=c(0,0,0,0,1,1,1,0,1,0),d=c(0,0,1,1,1,1,0,0,0,1), e=c(0,0,1,1,1,1,0,0,0,0))
kassess(kst, rpatterns=rp, method="deterministic")
}
\keyword{math}