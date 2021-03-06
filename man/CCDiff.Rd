\name{CCDiff}
\alias{CCDiff}

\title{ Contrasts of parameters simulated in BUGS }
\description{
 Calculate linear combinations of parameters simulated in BUGS. This is a test version for internal use!
}
\usage{
CCDiff(bugs, dat, cmat = NULL, 
type = c("Dunnett", "Tukey", "Sequen", "Williams", "Changepoint"))
}

\arguments{
  \item{bugs}{ an object of class \code{bugs}, containing N values of the joint posterior distribution of in an N-times-P matrix named \code{muvec} }
  \item{dat}{ an object of \code{R2Bugsdat1w} }
  \item{cmat}{ a contrast matrix of dimensions M-times-P}
  \item{type}{ a single character string, which type of comparisons to perform, if \code{cmat} is not given; this argument is ignored if \code{cmat} ist specified }
}
\details{
 Testversion, for internal use.
}
\value{
An object of class "CCDiff", a list with elements

  \item{chains }{the N-times-M matrix of the transformed joint posterior distribution}
  \item{bugs }{the bugs object, as input}
  \item{dat }{the object of class "R2Bugsdat1w", as input}
  \item{cmat }{the M-times-P contrast matrix}

}


\keyword{internal}

