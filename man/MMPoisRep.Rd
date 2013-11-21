\name{MMPoisRep}
\alias{MMPoisRep}
\docType{data}
\title{ Simulated data for a simple mixed model with Poisson response}
\description{
  Simulated data with a fixed factor cult (4 levels), with 8 randomized replications each,
 a (fixed) factor time (6 levels), which are repeated measurements taken from the same experimental units.
 The 32 experimental (plotid) units differ in their mean response follwing a gaussian distribution.
 The response Y follows a Poisson distribution. 
}
\usage{data(MMPoisRep)}
\format{
  A data frame with 192 observations on the following 4 variables.
  \describe{
    \item{\code{plotid}}{a factor with 32 levels, representing the 32 experimental units (plots)}
    \item{\code{cult}}{a factor with 4 levels (\code{C1} \code{C2} \code{C3} \code{C4}), representing a fixed factor (e.g. the cultivar)}
    \item{\code{time}}{a factor with 6 levels (\code{T1} \code{T2} \code{T3} \code{T4} \code{T5} \code{T6}) specifying repeated measurements on the same experimental units (plotid) over time}
    \item{\code{Y}}{a numeric vector, following a Poisson distribution}
  }
}
\source{
simulation
}
\examples{
data(MMPoisRep)

boxplot(Y ~ cult*time, data=MMPoisRep, las=TRUE)

}
\keyword{datasets}
