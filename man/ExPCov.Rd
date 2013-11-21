\name{ExPCov}
\alias{ExPCov}
\docType{data}
\title{ Simulated example data following a Poisson distribution}
\description{
Reponse follows a Poisson distribution, the covariables follow a multivariate normal on the log-scale.
}
\usage{data(ExPCov)}
\format{
  A data frame with 32 observations on the following 12 variables.
  \describe{
    \item{\code{resp}}{a response of counts}
    \item{\code{A}}{a factor with levels \code{A1} \code{A2} \code{A3} \code{A4}, e.g. varieties}
    \item{\code{C1}}{a numeric covariable}
    \item{\code{C2}}{a numeric covariable}
    \item{\code{C3}}{a numeric covariable}
    \item{\code{C4}}{a numeric covariable}
    \item{\code{C5}}{a numeric covariable}
    \item{\code{C6}}{a numeric covariable}
    \item{\code{C7}}{a numeric covariable}
    \item{\code{C8}}{a numeric covariable}
    \item{\code{C9}}{a numeric covariable}
    \item{\code{C10}}{a numeric covariable}
  }
}
\examples{
data(ExPCov)

boxplot(resp ~ A, data=ExPCov)

pairs(ExPCov)

}

\keyword{datasets}
