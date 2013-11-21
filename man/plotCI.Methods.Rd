\name{plotCI.Methods}
\alias{plotCI.UnlogCI}
\alias{plotCI.simplesimint}

\title{ Plot confidence intervals calculated by pairwiseCI }
\description{
Plot confidence intervals calculated by calling pairwiseCI or UnlogCI.
}
\usage{
\method{plotCI}{UnlogCI}(x, ...)
\method{plotCI}{simplesimint}(x, ...)
}

\arguments{
  \item{x}{ an object of class \code{"UnlogCI"} \code{"simplesimint"} as can be obtained by calling \code{\link{UnlogCI}} or \code{\link{simplesimint}} }
  \item{\dots}{ further arguments to be passed to \code{plotCII}, see \code{?plotCII} in package \pkg{MCPAN} for details }
}

\value{
A plot.
}



\keyword{ hplot}

