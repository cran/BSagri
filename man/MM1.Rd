\name{MM1}
\alias{MM1}
\docType{data}
\title{Simulated data set for a simple mixed model}
\description{
  Simulated data set for a simple mixed model
}
\usage{data(MM1)}
\format{
  A data frame with 160 observations on the following 3 variables.
  \describe{
    \item{\code{Y}}{a numeric vector, the response, sampled from a normal distribution}
    \item{\code{F}}{a factor with levels \code{F1} \code{F2} \code{F3} \code{F4}, representing fixed effects}
    \item{\code{R}}{a factor with levels \code{R1} \code{R2} \code{R3} \code{R4} \code{R5}, representing random effects, sampled from a normal distribution}
  }
}


\examples{
data(MM1)
boxplot(Y~F*R, data=MM1)
}
\keyword{datasets}
