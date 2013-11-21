\name{CCRatio.default}
\alias{CCRatio.default}

\title{ Compute ratio contrasts of chains of joint empirical distributions. }
\description{
Compute ratio contrasts of chains of joint empirical distributions.
For internal use.
}

\usage{
CCRatio.default(x, cmat)
}

\arguments{
  \item{x}{ an N times K matrix with numeric entries }
  \item{cmat}{ a list with entries \code{numC} and \code{denC},
 each containing a contrast matrix with K columns (must be the same K as in x!)
 and M rows (is your choice, the number of contrasts of interest) }
}


\details{Denote the elements of \code{x} by \eqn{x_{nk}}{x[n,k]}.
 Denote the numetator of \code{cmat} by \eqn{C}{C} 
with elements \eqn{c_{mk}}{c[m,k]} and the denominator of \code{cmat} as
\eqn{D}{D} with elements \eqn{d_{mk}}{d[m,k]}.
 Function \code{CCRatio.default} simply calculates
\deqn{\frac{\sum_{k=1}^{K}c_{mk}x_{nk}}{\sum_{k=1}^{K}d_{mk}x_{nk}}}{sum(k=1,..,K; m; n)(c[m,k]*x[n,k]) / sum(k=1,..,K; m; n)(d[m,k]*x[n,k])} for each m=1,...,M and n=1,...,N.
 The result is a N times M matrix.
}


\examples{


X<-round(cbind(
 rnorm(10,1,1),
 rnorm(10,1,1),
 rnorm(10,1,1),
 rnorm(10,1,1)))

# and numerator and denominator
# x times 4 contrast matrix

NMAT<-rbind(
c(1,0,0,0),
c(1,0,0,0),
c(1,0,0,0)
)


DMAT<-rbind(
c(0,1,0,0),
c(0,0,1,0),
c(0,0,0,1)
)


CCRatio.default(x=X, cmat=list(numC=NMAT, denC=DMAT) )


}

\keyword{ internal }

