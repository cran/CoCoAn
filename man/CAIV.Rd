\name{CAIV}
\alias{CAIV}
\title{Function to perform correspondence analysis with (or without) respect to instrumental variables}
\description{
  Multivariate analysis. This function perform correspondence analysis
  or constrained correspondence analysis.  This latter is better known
  under the name of canonical correspondence analysis.  This analysis
  finds coefficients of variables to obtain a row score of unit variance.
  This row score is used to compute by weighted averaging a column score
  of maximized variance.
}
\usage{
CAIV(L, E=diag(1, dim(L)[1], dim(L)[1]), normE=TRUE)
}
\arguments{
  \item{L}{ a (i,j) matrix of non-negative number }
  \item{E}{ an (i,p) optional matrix of p external variables}
  \item{normE}{ \code{TRUE} to normalize variables in matrix \code{E},
    \code{FALSE} otherwise}
}
\details{
  This function compute correspondence analysis (enter \code{L})
  or constrained
  correspondence analysis (enter \code{L} and \code{E}).  The function return the
  coefficient (B) to compute a row score of unit variance (R) that
  maximize the between-column inertia (column score in F obtained by
  weighting averaging).  D contains the intra-set covariance (correlation
  if \code{normE=TRUE}).  For correspondence analysis, \code{CAIV(t(L))}
  gives a column
  score of unit variance that maximize the between-rows inertia. Note that
  this function does not use convenient rescaling and so is a little bit
  different of ter Braak's CCA. (We use the algorithm of Chessel et al.)
}
\value{
  A list with components
  \item{ev}{a vector containing eigenvalues}
  \item{B}{coefficients of variables of \code{E} (only in constrained analysis)}
  \item{D}{covariance matrix between external variables and row scores
    (only in constrained analysis)}
  \item{R}{row coordinates of unit variance}
  \item{F}{column coordinates of variance \code{ev[i]} }
}
\references{
  ter Braak (1986): Canonical correspondence analysis: a new eigenvector
  technique for multivariate direct gradient analysis. \emph{Ecology}
  \bold{67(5)}, 1167--1179.
  
  Chessel, Lebreton and Yoccoz (1987): Propriétés de l'analyse canonique
  des correspondances; une illustration en hydrobiologie.
  \emph{Revue de Statistique Appliquée} \bold{35(4)} 55--72.}
\author{
  Stephane DRAY
  \email{dray@biomserv.univ-lyon1.fr}
}
\seealso{
  \code{\link{CAIV.plot}}
}

\examples{
##correspondence analysis
L <- matrix(c(4,2,0,2,0,5,1,3,2,4,0,2,2,0,3,1),4,4)
CAIV(L)
CAIV(t(L))
##canonical correspondence analysis
E <-matrix(c(1.5,2.3,2,1.6,0.9,0.8,1.2,1.5),4,2)
CAIV(L,E)}

\keyword{multivariate}
