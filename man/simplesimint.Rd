\name{simplesimint}
\alias{simplesimint}

\title{ Simultaneous confidence intervals from raw estimates}
\description{
Calculates simultaneous confidence intervals for multiple contrasts based on a parameter vector,
its variance-covariance matrix and (optionally) the degrees of freedom, using quantiles of the multivar

}
\usage{
simplesimint(coef, vcov, cmat, df = NULL, conf.level = 0.95,
 alternative = c("two.sided", "less", "greater"))
}

\arguments{
  \item{coef}{ a single numeric vector, specifying the point estimates of the parameters of interest }
  \item{vcov}{ the variance-covariance matrix corresponding to \code{coef}, should be of dimension P-times-P, when \code{coef} is of P }
  \item{cmat}{ the contrasts matrix specifying the comparisons of interest with respect to \code{coef}, should have P columns, when \code{coef} is of length p }
  \item{df}{ optional, the degree of freedom for the multivariate t-distribution; if specified, quantiles from the multivariate t-distribution are used for confidence interval estimation, if not specified (default), quantiles of the multivariate normal distribution are used}
  \item{conf.level}{ a single numeric value between 0.5 and 1.0; the simultaneous confidence level }
  \item{alternative}{ a single character string, \code{"two.sided"} for intervals, \code{"less"} for upper limits, and \code{"greater"} for lower limits }
}
\details{

Implements the methods formerly available in package \pkg{multcomp}, function \code{csimint}.
Input values are a vector of parameter estimates \eqn{\mu}{mu} of length \eqn{P}{P},
 a corresponding estimate for its variance-covariance matrix \eqn{\Sigma}{Sigma} (P times P), and a 
contrast matrix \eqn{C} of dimension \eqn{M \times P}{M times P}. The contrasts \eqn{L = C \mu}{L = C * mu} are computed,
 the variance-covariance matrix (being a function of \eqn{C}{C} and \eqn{\Sigma}{Sigma}) and the corresponding correlation matrix \eqn{R}{R} are computed.
Finally, confidence intervals for \eqn{L}{L} are computed: if df is given, quantiles of an M-dimensional t distribution with correlation matrix R are used,
otherwise quantiles of an M-dimensional standard normal distribution with correlation matrix R are used.
 
}
\value{
 An object of class "simplesimint"
  \item{estimate }{the estimates of the contrasts}
  \item{lower }{the lower confidence limits}
  \item{upper }{the upper confidence limits}
  \item{cmat}{the contrast matrix, as input}
  \item{alternative}{a character string, as input}
  \item{conf.level}{a numeric value, as input}
  \item{quantile}{a numeric value, the quantile used for confidence interval estimation}
  \item{df}{a numeric value or NULL, as input}
  \item{stderr}{the standard error of the contrasts}
  \item{vcovC}{the variance covariance matrix of the contrasts}

}

\author{ Frank Schaarschmidt }
\note{ 
This is a testversion and has not been checked extensively. 

}
\seealso{ See \code{?coef} and \code{?vcov} for extracting of parameter vectors and corresponding variance covariance matrices from various model fits. }
\examples{


# For the simple case of Gaussian response
# variables with homoscedastic variance,
# see the following example


library(mratios)
data(angina)

boxplot(response ~ dose, data=angina)

# Fit a cell means model,

fit<-lm(response ~ 0+dose, data=angina)

# extract cell means, the corresponding
# variance-covariance matrix and the
# residual degree of freedom,

cofi<-coef(fit)
vcofi<-vcov(fit)
dofi<-fit$df.residual

# define an appropriate contrast matrix,
# here, comparisons to control

n<-unlist(lapply(split(angina$response, f=angina$dose), length))
names(n)<-names(cofi)

cmat<-contrMat(n=n, type="Dunnett")
cmat

#

test<-simplesimint(coef=cofi, vcov=vcofi, df=dofi, cmat=cmat, alternative="greater" )

test

summary(test)

plotCI(test)

### Note, that the same result can be achieved much more conveniently
### using confint.glht in package multcomp


}

\keyword{ htest }
