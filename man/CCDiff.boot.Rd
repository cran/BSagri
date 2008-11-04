\name{CCDiff.boot}
\alias{CCDiff.boot}

\title{ Compute contrasts of chains of joint empirical distributions. }
\description{
Compute contrasts of chains of joint empirical distributions obtained by stratified bootstrap.
For internal use.
}
\usage{
CCDiff.boot(x, cmat = NULL,
 type = c("Dunnett", "Tukey", "Sequen", "Williams",
 "Changepoint", "McDermott", "GrandMean", "Marcus"))
}

\arguments{
  \item{x}{ an object of class "boot" as can be obtained by callinf \code{boot} in package \pkg{boot} with stratified bootstrap }
  \item{cmat}{ an optional contrast matrix, ncol(cmat) should be the same the number of strata in x }
  \item{type}{ a single character string, naming a contrast type available in \code{contrMat} in package \pkg{multcomp}, see \code{?contrMat} for details }
}
\details{
 Testversion. For internal use.
}


\keyword{ htest }
\concept{ contrast }
