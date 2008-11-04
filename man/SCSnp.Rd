\name{SCSnp}
\alias{SCSnp}
\alias{SCSnp.default}
\alias{SCSnp.CCDiff}
\alias{SCSnp.CCRatio}
\alias{SCSnp.bugs}

\title{ Simultaneous confidence sets from empirical joint distribution. }
\description{
Calcualte simultaneous confidence sets according to Besag et al. (1995) from a empirical joint distribution of a parameter vector. Joint empirical distributions might be obtained from WinBUGS or OpenBUGS calls.
}
\usage{

SCSnp(x,...)

\method{SCSnp}{default}(x, conf.level = 0.95,
 alternative = "two.sided", ...)

\method{SCSnp}{bugs}(x, conf.level = 0.95,
 alternative = "two.sided", whichp = NULL, ...)

\method{SCSnp}{CCRatio}(x, ...)

\method{SCSnp}{CCDiff}(x, ...)
}
\arguments{
  \item{x}{ a matrix N-times-P matrix or an object of class \code{CCRatio} or \code{CCDiff} }
  \item{conf.level}{ a single numeric value between 0.5 and 1, the simultaneous confidence level}
  \item{alternative}{ a single character string, one of \code{"two.sided"}, \code{"less"}, \code{"greater"}, for two-sided, upper and lower limits  }
  \item{whichp}{ a single character string, naming an element of the \code{sims.list} if \code{x} is a \code{bugs} object, ignored otherwise }
  \item{\dots}{ further arguments, currently not used }
}
\details{
Let P be the number of parameters in the parameter vector and N be the total number of values obtained for the empirical joint distribution of the parameter vector, e.g. as can be obtaine e.g., from Gibbs sampling.
}
\value{
  An object of class "SCSnp", a list with elements

  \item{conf.int }{a P-times-2 matrix containing the lower and upper confidence limits}
  \item{estimate }{a numeric vector of length P, containing the medians of the P marginal empirical distributions}
  \item{x }{ the input object}
  \item{k }{ the number of values outside the SCS, i.e. conf.level*N}
  \item{N }{ the number of values used to construct the confidence set}
  \item{conf.level }{a single numeric value, the nominal confidence level, as input}
  \item{alternative }{a single character string, as input}
}
\references{ Besag J, Green P, Higdon D, Mengersen K (1995): Bayesian Computation and Stochastic Systems. Statistical Science 10 (1), 3-66.}
\author{ Frank Schaarschmidt }

\seealso{ \code{\link{CInp}} for a wrapper to \code{quantile} to compute elementwise intervals}
\examples{


# Assume a 1000 times 4 matrix of 4 mutually independent
# normal variables:

X<-cbind(rnorm(1000), rnorm(1000), rnorm(1000), rnorm(1000))

SCSts<-SCSnp(x=X, conf.level=0.9, alternative="two.sided")
SCSts

SCS<-SCSts$conf.int

in1<-X[,1]>=SCS[1,1] & X[,1]<=SCS[1,2] 

in2<-X[,2]>=SCS[2,1] & X[,2]<=SCS[2,2] 

in3<-X[,3]>=SCS[3,1] & X[,3]<=SCS[3,2] 

in4<-X[,4]>=SCS[4,1] & X[,4]<=SCS[4,2] 

sum(in1*in2*in3*in4)


}

\keyword{ htest }
\concept{ confidence set }
