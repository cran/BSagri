\name{summary.simplesimint}
\alias{summary.simplesimint}

\title{ Detailed print out for simplesimint objects }
\description{
 Produce a detailed print out for objects of class "simplesimint".
}
\usage{
\method{summary}{simplesimint}(object, ...)
}

\arguments{
  \item{object}{an object of class \code{"simplesimint"} as can be obtained by calling \code{\link{simplesimint}} }
  \item{\dots}{ further arguments to be passed to \code{print} }
}

\value{
Prints to console.
}

\examples{
library(multcomp)
data(waste)

anova(lm(waste ~ temp*envir, data=waste))

# a cell-means model instead
# of a two-way-layout

fit <- lm(waste ~ 0 + temp:envir, data=waste)

COEF <- coef(fit)
VCOV <- vcov(fit)

DF <- fit$df.residual

IAC <- IAcontrasts(type=c("Identity", "Dunnett"), k=c(5,3))
IACn <- c2compnames(IAC)

SCI<- simplesimint(coef=COEF, vcov=VCOV, df=DF, cmat=IACn)

# the print function just gives this:

SCI

# the summary function produces some more output:

summary(SCI)

###############


IAC <- IAcontrasts(type=c("Identity", "Dunnett"), k=c(5,3))

# long names currently give nasty outputs!

colnames(IAC)<-names(COEF)

IACn <- c2compnames(IAC)

SCI<- simplesimint(coef=COEF, vcov=VCOV, df=DF, cmat=IACn)

SCI

# the summary function produces some more output:

summary(SCI)


}

\keyword{ print }

