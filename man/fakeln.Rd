\name{fakeln}
\alias{fakeln}
\docType{data}
\title{A simulated data set of lognormal data}
\description{
 A simulated data set of lognormal data, could be concentrations 
}
\usage{data(fakeln)}
\format{
  A data frame with 32 observations on the following 2 variables.
  \describe{
    \item{\code{Concmug}}{a numeric vector, serving as response variable}
    \item{\code{Treat}}{a factor with levels \code{N} \code{S} \code{Sa} \code{Sb}}
  }
}

\examples{
data(fakeln)
boxplot(Concmug ~ Treat, data=fakeln)
}
\keyword{datasets}
