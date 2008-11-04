
\name{CCRatio.boot}
\alias{CCRatio.boot}

\title{ Compute ratio contrasts of chains of joint empirical distributions. }
\description{
Compute contrasts of chains of joint empirical distributions.
For internal use.
}
\usage{
CCRatio.boot(x, cmat = NULL,
 type = c("Dunnett", "Tukey", "Sequen", "Williams",
 "Changepoint", "McDermott", "GrandMean", "Marcus"))
}

\arguments{
  \item{x}{ an object of class "boot" as can be obtained by callinf \code{boot} in package \pkg{boot} with stratified bootstrap }
  \item{cmat}{ an optional list of two contrast matrices, in entries \code{numC} and \code{denC} of the list;
 ncol(numC) and ncol(denC) should be the same the number of strata in x }
  \item{type}{ a single character string, naming a contrast type available in \code{contrMatRatio} in package \pkg{mratios}, see \code{?contrMatRatio} for details }
}

\details{
 Testversion. For internal use.
}

\keyword{ htest }
\concept{ contrats }
