\name{CIGLM}
\alias{CIGLM}

\title{ Wrapper to compute confidence intervals from glms }
\description{
 Computes confidence intervals from the output of a glm, by calling to glht(multcomp).
}
\usage{
CIGLM(x, conf.level = 0.95, method = c("Raw", "Adj", "Bonf"))
}
\arguments{
  \item{x}{ a object of class \code{"glm"}, \code{"gamlss"}, or \code{"glm.nb"} as can be obtained by calling to function \code{glm}, function \code{gamlss} in package \code{gamlss}, or function \code{glm.nb} in package \code{MASS}  }
  \item{conf.level}{  confidence level, a single numeric value between 0.5 and 1 }
  \item{method}{ a single character string, with \code{"Raw"} for unadjusted intervals, \code{"Adj"} for multiplicity adjusted intervals taking the correlations into account, or \code{"Bonf"} for a Bonferroni adjustment }
}
\details{
 This is just a wrapper to \code{confinf.glht} of package \code{multcomp}.

 Note that except for the simple general linear model with assumption of Gaussian response, the resulting intervals are exact intervals.
 In other cases, the methods are only asymptotically correct, hence might give misleading results for small sample sizes!
}
\value{
An object of class \code{"confint.glht"}
}

\seealso{ \code{confint.glht} in package \code{multcomp} for the function that is used internally,
\code{\link{UnlogCI}} for a simple function to bring confidence intervals back to the original scales
when there is a log or logit link, with appropriate naming.
 }
\examples{

data(Diptera)
library(multcomp)


modelfit <- glm(Ges ~ Treatment, data=Diptera, family=quasipoisson)
comps <- glht(modelfit, mcp(Treatment="Tukey"))
CIs<-CIGLM(comps, method="Raw")
CIs

CIsAdj<-CIGLM(comps, method="Adj")
CIsAdj

CIsBonf<-CIGLM(comps, method="Bonf")
CIsBonf


library(gamlss)

modelfit2 <- gamlss(Ges ~ Treatment, data=Diptera, family=NBI)
comps2 <- glht(modelfit2, mcp(Treatment="Tukey"))
CIs2<-CIGLM(comps2, method="Raw")
CIs2

CIsAdj2<-CIGLM(comps2, method="Adj")
CIsAdj2

CIsBonf2<-CIGLM(comps2, method="Bonf")
CIsBonf2



}
\keyword{ htest }