\name{CAIV.plot}
\alias{CAIV.plot}
\title{Biplot or Triplot after using the CAIV function}
\description{
  \code{CAIV} performs correspondence analysis or
  constrained correspondence analysis.
  \code{CAIV.plot} allows a two-dimensional representation (biplot) of row,
  column and coefficients of variables.
}
\usage{
CAIV.plot(obj, x=1, y=2, add.row=TRUE, add.col=TRUE, add.var=FALSE,
          row.names="", col.names="", var.names="")
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{obj}{an object created by the CAIV function}
  \item{x}{an integer that defines which score corresponds to x axis}
  \item{y}{an integer that defines which score corresponds to y axis}
  \item{add.row}{TRUE to add rows on biplot FALSE otherwise}
  \item{add.col}{TRUE to add columns on biplot FALSE otherwise}
  \item{add.var}{TRUE to add variables on biplot FALSE otherwise (only for constrained analysis)}
  \item{row.names}{a vector of strings containing row names (Ri otherwise)}
  \item{col.names}{a vector of strings containing columns names (Ci otherwise)}
  \item{var.names}{a vector of strings containing variables names (Vari otherwise)}
}
\author{
  Stephane DRAY
  \email{dray@biomserv.univ-lyon1.fr}
}

\seealso{
  \code{\link{CAIV}}
}

\examples{
## correspondence analysis
L <- matrix(c(4,2,0,2,0,5,1,3,2,4,0,2,2,0,3,1),4,4)
CAIV.plot(CAIV(L))
CAIV.plot(CAIV(L),row.names=c("a","b","c","d"))
## canonical correspondence analysis
E <- matrix(c(1.5,2.3,2,1.6,0.9,0.8,1.2,1.5),4,2)
CAIV.plot(CAIV(L,E))
CAIV.plot(CAIV(L,E),add.var=TRUE)

}
\keyword{multivariate}
\keyword{hplot}
