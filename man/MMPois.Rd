\name{MMPois}
\alias{MMPois}
\docType{data}
\title{ Simulated data for a simple mixed model with Poisson response}
\description{
  A fixed factor F (four levels) and a random factor (five levels), modifying the mean response (random Intercept)
 Y is a variable following a Poisson distribution.
}
\usage{data(MMPois)}
\format{
  A data frame with 160 observations on the following 3 variables.
  \describe{
    \item{\code{Y}}{a numeric vector, the Poisson distributed response.}
    \item{\code{F}}{a factor with levels \code{F1} \code{F2} \code{F3} \code{F4}}
    \item{\code{R}}{a factor with levels \code{R1} \code{R2} \code{R3} \code{R4} \code{R5}}
  }
}
\source{
  simulation
}
\examples{
data(MMPois)

boxplot(Y~R*F, data=MMPois, las=2)

}
\keyword{datasets}
