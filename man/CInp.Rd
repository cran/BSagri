\name{CInp}
\alias{CInp}
\alias{CInp.default}
\alias{CInp.CCRatio}
\alias{CInp.CCDiff}
\alias{CInp.bugs}


\title{ Construct local confidence intervals from joint empirical distribution. }
\description{
Construct local confidence intervals for each parameter from the empirical joint distribution of 
a parameter vector of length P. 
}
\usage{
CInp(x, ...)

\method{CInp}{default}(x, conf.level = 0.95,
 alternative = "two.sided", ...)

\method{CInp}{CCRatio}(x, ...)

\method{CInp}{CCDiff}(x, ...)

\method{CInp}{bugs}(x, conf.level = 0.95,
 alternative = "two.sided", whichp = NULL, ...)


}

\arguments{
  \item{x}{ an N-times-P matrix, or an object of class \code{CCRatio}, \code{CCDiff}, \code{bugs}, as can be obtained by calling the functions \code{\link{CCRatio}}, \code{\link{CCDiff}}, or \code{openbugs} in package \code{R2WinBUGS} }

  \item{conf.level}{ a single numeric value between 0.5 and 1, specifying the local confidence level for each of the P parameters }
  \item{alternative}{ a single character string, one of \code{"two.sided"}, \code{"less"}, \code{"greater"}, for two-sided, upper and lower limits  }
  \item{whichp}{ a single character string, naming an element of the \code{sims.list} if \code{x} is a \code{bugs} object, ignored otherwise }
  \item{\dots}{currently not used }
}
\details{
 Construct simple confidence intervals based on order statistics applied to the marginal empirical distributions in \code{x}.
}

\value{
  An object of class "CInp", a list with elements

  \item{conf.int }{a P-times-2 matrix containing the lower and upper confidence limits}
  \item{estimate }{a numeric vector of length P, containing the medians of the P marginal empirical distributions}
  \item{x }{ the input object}
  \item{k }{ the number of values outside each confidence interval, i.e. conf.level*N}
  \item{N }{ the number of values used to construct each confidence interval}
  \item{conf.level }{a single numeric value, the nominal confidence level, as input}
  \item{alternative }{a single character string, as input}

}

\seealso{ The function internally used is \code{\link{quantile}} with its default settings. 
 See \code{\link{SCSnp}} for simultaneous sets.}

\examples{

# Assume a 100 times 4 matrix of 4 mutually independent
# normal variables:

X<-cbind(rnorm(100), rnorm(100), rnorm(100), rnorm(100))

lcits<-CInp(x=X, conf.level=0.95, alternative="two.sided")
lcits

ci1<-lcits$conf.int[1,]
length( which(X[,1]>=ci1[1] & X[,1]<=ci1[2] ) )


ci2<-lcits$conf.int[2,]
length( which(X[,2]>=ci2[1] & X[,2]<=ci2[2] ) )


}

\keyword{ htest }
\concept{ confidence interval }
