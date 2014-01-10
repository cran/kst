\name{kvalidate}
\alias{kvalidate}
\title{Validate Prerequisite Relations or Knowledge Structures}
\description{
  Validates prerequisite relations or knowledge structures
}
\usage{
  kvalidate(x, rpatterns=NULL, method=c("gamma","percent","VC","DA"))
}
\arguments{
  \item{x}{An \R object of class \code{\link{kstructure}}.}
  \item{rpatterns}{A binary data frame or matrix where each row specifies
  the response pattern of one individual to the set of domain problems in x.}
  \item{method}{The desired validation method (see details).}
}
\details{
  \code{kvalidate} calculates validity coefficients for prerequisite
  relations and knowledge structures.

  The \emph{\eqn{\gamma}{gamma}-Index} (method \code{"gamma"}) validates
  the prerequisite relation underlying a knowledge structure and assumes
  that not every response pattern is represented by a prerequisite relation.
  For this purpose it compares the number of response patterns that are
  represented by a prerequisite relation (i.e., concordant pairs) with the
  number of response patterns that are not represented by a prerequisite
  relation (i.e., discordant pairs). Formally, the \eqn{\gamma}{gamma}-Index
  is defined as \deqn{\gamma = \frac{N_c - N_d}{N_c + N_d}}{gamma = (Nc-Nd)/(Nc+Nd)}
  where \eqn{N_c}{Nc} is the number of concordant pairs and \eqn{N_d}{Nd}
  the number of discordant pairs. Generally, a positive \eqn{\gamma}{gamma}-value
  supports the validity of prerequisite relations.

  The validation method \code{"percent"} likewise validates prerequisite
  relations and assumes that more difficult or complex domain problems are
  solved less frequently than less difficult or complex domain problems.
  For this purpose it calculates the relative solution frequency for each
  of the domain problems in \emph{Q}.

  The \emph{Violational Coefficient} (method \code{"VC"}) also validates
  prerequisite relations. For this purpose, the number of violations
  (i.e., the earlier mentioned discordant pairs) against a prerequisite
  relation are calculated. Formally, the VC is defined as
  \deqn{VC = \frac{1}{n(|S| - m)} \sum_{x,y} v_{xy}}{VC = 1/(n*(|S|-m))*sum(vxy)}
  where \eqn{n} denotes the number of response vectors, \eqn{|S|}
  refers to the number of pairs in the relation, \eqn{m} denotes the number
  of items, and \eqn{v_{xy}}{vxy} again refers to the number of discordant
  pairs. Generally, a low VC supports the validity of prerequisite relations.

  In contrast to the other three indices, the \emph{Distance Agreement
  Coefficient} (method \code{"DA"}) validates the resulting knowledge
  structure. For this purpose it compares the average symmetric distance
  between the knowledge structure and respone patterns (referred to as
  \emph{ddat}) to the average symmetric distance between the knowledge
  structure and the power set of response patterns (referred to as
  \emph{dpot}). By calculating the ratio of \emph{ddat} and \emph{dpot},
  the DA is determined. Generally, a lower DA-value indicates a better fit
  between a knowledge structure and a set of response patterns.
}
\value{
  Depending on the desired assessment method, a numeric value (methods
  \code{"gamma"} and \code{"VC"}), a data frame with results for
  each domain problem (method \code{"percent"}), or a list
  (method \code{"DA"}) with the following components:
  \item{ddat}{The ddat-value.}
  \item{ddat_dist}{The distance table for ddat.}
  \item{dpot}{The dpot-value.}
  \item{dpot_dist}{The distance table for dpot.}
  \item{DA}{The Distance Agreement Coefficient.}
}
\seealso{
  \code{\link{kstructure}}
}
\references{
  Goodman, L. A. & Kruskal, W. H. (1972) Measures of association for
  cross classification. \emph{Journal of the American Statistical
  Association}, 67.

  Schrepp, M. (1999) An empirical test of a process model for letter
  series completion problems. In D. Albert & J. Lukas (Eds.),
  \emph{Knowledge Spaces: Theories, Emprical Research, Applications}.
  Mahwah, NJ: Lawrence Erlbaum Associates.

  Schrepp, M., Held, T., & Albert, D. (1999) Component-based
  construction of surmise relations for chess problems. In D. Albert &
  J. Lukas (Eds.), \emph{Knowledge Spaces: Theories, Empirical Research,
  Applications}. Mahwah, NJ: Lawrence Erlbaum Associates.
}
\examples{
kst <- kstructure(set(set("a"), set("a","b"), set("a","c"), set("d","e"), 
   set("a","b","d","e"), set("a","c","d","e"), set("a","b","c","d","e")))
rp <- data.frame(a=c(1,1,0,1,1,1,1,0,0,0),b=c(0,1,0,1,0,1,0,1,0,0),
   c=c(0,0,0,0,1,1,1,0,1,0),d=c(0,0,1,1,1,1,0,0,0,1), e=c(0,0,1,1,1,1,0,0,0,0))

# Gamma Index
kvalidate(kst, rpatterns=rp, method="gamma")

# Percent
kvalidate(kst, rpatterns=rp, method="percent")

# Violational Coefficient
kvalidate(kst, rpatterns=rp, method="VC")

# Distance Agreement Coefficient
kvalidate(kst, rpatterns=rp, method="DA")
}
\keyword{math}