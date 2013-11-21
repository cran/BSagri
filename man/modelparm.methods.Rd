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
\method{modelparm}{gamlss}(model, coef.=coef, vcov.=vcov, df = NULL, ...)
\method{modelparm}{geeglm}(model, coef.=coef, vcov.=vcov, df = NULL, ...)
}

\arguments{
  \item{model}{ an object of class \code{gamlss} and \code{geeglm}, respectively}
  \item{coef.}{ a function to extract fitted mean parameters from models of class \code{gamlss} and \code{geeglm}}
  \item{vcov.}{ a function to extract the estimated covariance matrix from models of class \code{gamlss} and \code{geeglm}}
  \item{df}{ a single positive integer, the user-defined degrees of freedom. By default, \code{df=NULL}, the funtion will extract the residual degrees of freedom from the model.}
  \item{\dots}{ further argument, not used} 
}

\details{
 Test versions. Only for internal use. Further checks still need to be implemented.
}

\value{
As modelparm.default in package multcomp, a list with elements 
  \item{coef}{a numeric vector with the estimated model parameters}
  \item{vcov}{a matrix with numeric entries, and dimensions as length(coef)}
  \item{df}{a single numeric value, the residual degree of freedom}
  \item{estimable}{a logical vector, specifying which rows/colums of vcov correspond to non-NA parameters}
  }

\author{Daniel Gerhard, Frank Schaarschmidt adapting code by T.Hothorn available in \code{modelparm.default}, package \code{multcomp}} 

\seealso{ function \code{glht} in package \code{multcomp} }

\keyword{ internal }