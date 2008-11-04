\name{plotCI.Methods}
\alias{plotCI.pairwiseCI}
\alias{plotCI.UnlogCI}
\alias{plotCI.simplesimint}

\title{ Plot confidence intervals calculated by pairwiseCI }
\description{
Plot confidence intervals calculated by calling pairwiseCI or UnlogCI.
}
\usage{
\method{plotCI}{pairwiseCI}(x, ...)
\method{plotCI}{UnlogCI}(x, ...)
\method{plotCI}{simplesimint}(x, ...)
}

\arguments{
  \item{x}{ an object of class \code{"pairwiseCI"}, \code{"UnlogCI"} \code{"simplesimint"} as can be obtained by calling \code{pairwiseCI} in package \pkg{pairwiseCI}, by calling \code{\link{UnlogCI}} or \code{\link{simplesimint}} }
  \item{\dots}{ further arguments to be passed to \code{plotCII}, see \code{?plotCII} in package \pkg{MCPAN} for details }
}

\value{
A plot.
}


\examples{

data(Cica1)
boxplot(Au_Bonitur~Treatment, data=Cica1)

ZikaNP<-pairwiseCI(Au_Bonitur~Treatment, data=Cica1,
 method="HL.ratio", control="Iso", alternative="two.sided", conf.level=0.9)

plotCI(ZikaNP, lines=c(0.5,1,2), lineslty=c(3,1,3), lineslwd=c(1,2,1),
 ylim=c(0.5,2.5), main="Auchenorhyncha visual assessment" )

####################


}

\keyword{ hplot}
\concept{confidence interval}
