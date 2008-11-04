\name{modelparm.methods}

\alias{modelparm.gamlss}
\alias{modelparm.geeglm}


\title{ For internal use. Extract model parameters needed for multcomp from objects of class gamlss
 or geeglm.}

\description{
  Only for internal use with \code{glht} in package \pkg{multcomp}.
  Extracts model parameters needed for \code{glht(multcomp)} from objects of 
class \code{gamlss} or \code{geeglm}.}

\usage{
\method{modelparm}{gamlss}(model, coef. = coef, vcov. = vcov.gamlss, df = NULL, ...)
\method{modelparm}{geeglm}(model, coef. = coef, vcov. = vcov.geeglm, df = NULL, ...)


}

\arguments{
  \item{model}{ an object of class \code{gamlss}, as currently is created by calling \code{gamlss} }
  \item{coef.}{ function to extract coefficients }
  \item{vcov.}{ function to extract variance covariance matrix }
  \item{df}{ function to extract degrees of freedom  }
  \item{\dots}{ further argument to be passed to \code{modelparm.default(multcomp)} }
}

\details{
  Only for internal use. Further checks should be implemented.
}

\value{
As \code{modelparm.default}.
  }

\author{Daniel Gerhard, code slightly changed from package \code{multcomp}} 

\seealso{ function \code{glht} in package \code{multcomp} }

\keyword{ htest }