\name{CountRep}
\alias{CountRep}
\docType{data}
\title{ Synthetic repeated count data set }
\description{
 Synthetic data set of repeated counts on the subject.
}
\usage{data(CountRep)}
\format{
  A data frame with 160 observations on the following 4 variables.
  \describe{
    \item{\code{Abundance}}{a numeric vector with counts simulated from an overdispersed and autocorrelated Poisson distributions}
    \item{\code{ID}}{a factor with levels \code{N1} \code{N2} ,..., \code{n40}, specifying the subject}
    \item{\code{Time}}{a factor with levels \code{T1} \code{T2} \code{T3} \code{T4}, specifying the time}
    \item{\code{Treatment}}{a factor with levels \code{N} \code{S1} \code{S2} \code{S3}}
  }
}
\details{
  Synthetic data set.
}

\examples{
data(CountRep)

library(lattice)

xyplot(Abundance ~ Time|Treatment, data=CountRep, subject=~ID)


}
\keyword{datasets}
