\name{allignment}
\alias{allignment}

\title{ Allignment according to one factor }
\description{
Substracts the mean or median from observations belonging to the same level of a factor.
}
\usage{
allignment(response, block, type = c("mean", "median"), ...)
}

\arguments{
  \item{response}{ a numeric vector }
  \item{block}{ a factor of the same length as \code{response} }
  \item{type}{ type of location measure to calculate and substract; only the choices "mean" and "median" are supported }
  \item{\dots}{ further arguments to be passed to \code{mean} or \code{median} }
}
\details{
Splits \code{response} according to the levels of \code{block}, calculates and substracts the mean or median and returns the resulting vector in appropriate order.
}
\value{
A numeric vector.
}

\keyword{ misc }
