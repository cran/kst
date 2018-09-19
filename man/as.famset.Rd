\name{as.famset}
\alias{as.famset}
\title{Convert a binary matrix to a family of sets}
\description{
  Create a \code{\link[sets]{set}} of \code{sets} from a binary
  matrix where each row of the matrix is taken as one set.
}
\usage{
  as.famset(m, as.letters = TRUE)
}
\arguments{
  \item{m}{A binary matrix.}
  \item{as.letters}{logical, should the elements of the sets be letters
                    or numbers?}
}
\details{
  \code{as.famset} takes a binary matrix and converts it to a family (i.e.
  \code{\link[sets]{set}}) of sets where each row of the matrix represents
  one set and a "1" in row i and column j means that element j is contained
  in set i.

  If \code{as.letters} is \code{TRUE} the elements of the sets are letters,
  otherwise numbers.

  If the matrix contains the same row multiple times it is contained only
  once in the resulting family of sets.
}
\value{
  An \R object of class \code{\link[sets]{set}} containing \code{sets}..
}
\seealso{
  \code{\link{as.binaryMatrix}}, \code{\link[sets]{set}}
}
\examples{
m <- matrix(c(1, 0, 0, 1, 1, 0), nrow = 2, ncol = 3)
m
as.famset(m)
as.famset(m, as.letters = FALSE)
}
\keyword{math}
