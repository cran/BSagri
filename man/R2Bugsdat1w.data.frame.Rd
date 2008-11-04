\name{R2Bugsdat1w.data.frame}
\alias{R2Bugsdat1w.data.frame}

\title{ For internal use. }
\description{
 Transform a data set to a dataset appropriate for certain OpenBUGS models.
}
\usage{
R2Bugsdat1w.data.frame(data, response, treatment, Intercept = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a data.frame }
  \item{response}{ a single character string, naming a numeric variable in \code{data} }
  \item{treatment}{ a single character string, naming a factor variable in \code{data}  }
  \item{Intercept}{ a single logical value, defining, whether an Intercept shall be used for the construction of the design matrix }
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
\keyword{misc }
