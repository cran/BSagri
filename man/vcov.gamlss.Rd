\name{vcov.gamlss}
\alias{vcov.gamlss}
\title{ Extract variance covariance matrix from objects of class gamlss }
\description{
Only for internal use. Extract the varaince covariance matrix corresponding to the mu parameters of a gamlss-fit.
 }
\usage{
\method{vcov}{gamlss}(object, \dots)
}
\arguments{
  \item{object}{ An object of class "gamlss" as can be created by calling \code{gamlss} }
  \item{\dots}{Currently not used.}
}
\details{
 Only for internal use. Needs implementation of warnings.
}
\value{A matrix of dimension mxm, if m is the length of mu-parameters from a gamlss fit.
  }
\author{ Daniel Gerhard }

\seealso{  packages \code{gamlss} and \code{multcomp} }

\keyword{ htest }