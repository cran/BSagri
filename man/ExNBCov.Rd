\name{ExNBCov}
\alias{ExNBCov}
\docType{data}
\title{ Simulated example data, drawn from a Negative Binomial Distribution}
\description{
  Simulated example data, response drawn from a Negative Binomial Distribution,
 Covariables follow a multivariate normal distribution.
}
\usage{data(ExNBCov)}
\format{
  A data frame with 32 observations on the following 12 variables.
  \describe{
    \item{\code{Resp}}{a numeric vector, a response of counts}
    \item{\code{Group}}{a factor with levels \code{A1} \code{A2} \code{A3} \code{A4}, e.g. varieties}
    \item{\code{X1}}{a numeric covariable}
    \item{\code{X2}}{a numeric covariable}
    \item{\code{X3}}{a numeric covariable}
    \item{\code{X4}}{a numeric covariable}
    \item{\code{X5}}{a numeric covariable}
    \item{\code{X6}}{a numeric covariable}
    \item{\code{X7}}{a numeric covariable}
    \item{\code{X8}}{a numeric covariable}
    \item{\code{X9}}{a numeric covariable}
    \item{\code{X10}}{a numeric covariable}
  }
}
\examples{
data(ExNBCov)

boxplot(Resp ~ Group, data=ExNBCov)

pairs(ExNBCov)

}
\keyword{datasets}
