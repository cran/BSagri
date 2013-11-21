\name{vcov.geeglm}
\alias{vcov.geeglm}
\title{ Extract variance covariance matrix from objects of class geeglm }
\description{
Only for internal use with function glht: Extract the variance covariance matrix corresponding to the mu parameters of a gamlss-fit.
Merely a wrapper of the method internally used in function summary.geeglm, package geepack.
 }
\usage{
\method{vcov}{geeglm}(object, \dots)
}
\arguments{
  \item{object}{ An object of class "geeglm" as can be created by calling \code{geeglm} in package \pkg{geepack}.}
  \item{\dots}{Currently not used.}
}
\details{
 Test version. Only for internal use. Needs implementation of warnings.
}
\value{A matrix of dimension m times m, if m is the length of coefficients from a geeglm fit.
  }

\seealso{  packages \code{gamlss} and \code{multcomp} }

\keyword{ htest }