\name{CountRep}
\alias{CountRep}
\docType{data}
\title{Simulated count data incl. repeated measurements}
\description{
 Simulated data set of repeated count data within subjects.
}
\usage{data(CountRep)}
\format{
  A data frame with 160 observations on the following 4 variables.
  \describe{
    \item{\code{Abundance}}{a numeric vector with counts simulated from overdispersed and autocorrelated Poisson distributions}
    \item{\code{ID}}{a factor with levels \code{N1} \code{N2} ,..., \code{n40}, specifying the subject}
    \item{\code{Time}}{a factor with levels \code{T1} \code{T2} \code{T3} \code{T4}, specifying the time}
    \item{\code{Treatment}}{a factor with levels \code{N} \code{S1} \code{S2} \code{S3}}
  }
}


\examples{
data(CountRep)


}
\keyword{datasets}
