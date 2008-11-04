\name{R2Bugsdat1w}
\alias{R2Bugsdat1w}
\title{ For internal use }
\description{
 Transform a data set to a dataset appropriate for certain OpenBUGS models.
}
\usage{
R2Bugsdat1w(formula, data)
}

\arguments{
  \item{formula}{ a formula of the style \code{response ~ treatment}, where \code{response} is a numeric or integer variable, and \code{treatment} is a factor variable in \code{data} }
  \item{data}{ a data.frame, containing the \code{response}- and \code{treatment}-variables in \code{formula} }
}
\details{
  For internal use.
}
\value{
  a list, containing the elements

  \item{bugsdat }{a list of variables appropriate for certain BUGS models}
  \item{parameters }{a vector of character strings, naming the parameters to save for a call to OpenBUGS}
  \item{inits }{ a vector of initial values for the parameters}
  \item{data }{the original data set}
  \item{Intercept }{a single logical indicating whether an Intercept was used to parameterize the factor variable}
}

\keyword{ misc }
