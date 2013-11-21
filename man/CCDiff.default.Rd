\name{CCDiff.default}
\alias{CCDiff.default}

\title{ Compute contrasts of chains of joint empirical distributions. }
\description{
Compute contrasts of chains of joint empirical distributions.
For internal use.
}

\usage{
CCDiff.default(x, cmat)
}

\arguments{
  \item{x}{ an N times K matrix with numeric entries }
  \item{cmat}{ a contrast matrix with K columns }
}
\details{Denote the elements of \code{x} by \eqn{x_{nk}}{x[n,k]} and denote the elements of \code{cmat} by
\eqn{c_{mk}}{c[m,k]}. Function \code{CCDiff.default} simply calculates:
\deqn{\sum_{k=1}^{K}c_{mk}x_{nk}}{sum(k=1,..,K; m; n)(c[m,k]*x[n,k])} for each m=1,...,M and n=1,...,N.
 The result is a N times M matrix.
}

\examples{

# What the function does:

# a 10 times 4 matrix

X<-round(cbind(
 rnorm(10,1,1),
 rnorm(10,1,1),
 rnorm(10,1,1),
 rnorm(10,1,1)))

# and a x times 4 contrast matrix

CMAT<-rbind(
c(-1,1,0,0),
c(-1,0,1,0),
c(-1,0,0,1)
)

CCDiff.default(x=X, cmat=CMAT)

}


\keyword{internal}

