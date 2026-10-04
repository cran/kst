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
  \item{m}{A binary matrix mof class \code{kmfamset}.}
  \item{as.letters}{logical, should the elements of the sets be letters
                    or numbers? Only used if colnames(m) is NULL.}
}
\details{
  \code{as.famset} takes a binary matrix and converts it to a family (i.e.
  \code{\link[sets]{set}}) of sets where each row of the matrix represents
  one set and a "1" in row i and column j means that element j is contained
  in set i.

  If \code{as.letters} is \code{TRUE} the elements of the sets are letters,
  otherwise numbers. However, if the matrix has colnames, these are taken
  as names for the elements of the sets taking precedence over the
  \code{as.letters} parameter.

  If the matrix contains the same row multiple times it is contained only
  once in the resulting family of sets.
}
\value{
  An \R object of class \code{\link[sets]{set}} containing \code{sets}. 
  Further class attributes arer set according to the class attributes of 
  \code{m}.
}
\seealso{
  \code{\link{as.binaryMatrix}}, \code{\link[sets]{set}}
}
\examples{
m <- matrix(c(1, 0, 0, 1, 1, 0), nrow = 2, ncol = 3)
class(m) <- unique(c("kmfamset", class(m)))
m
as.famset(m)
as.famset(m, as.letters = FALSE)
}
\keyword{math}
