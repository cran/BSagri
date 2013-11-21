\name{Brachycera}
\alias{Brachycera}
\docType{data}
\title{Eklektor counts of Brachycera}
\description{
  In a field trial, 4 treatments were arranged in a randomized complete block design with 8 blocks
 and 32 plots. Soil eklektor traps were placed in each plot, on six dates
 from 2005-07-12 to 2005-09-25, the number of individuals of Brachycera (Flies, Order Diptera)
 hatching from soil were counted. The individuals were classified to the family level. Interest
 was in assessing potential effects of the novel treatment (Novum) on the abundance of Brachycera, compared to a 
 near standard (Standard) and two additional standard treatments, A and B.
}

\usage{data(Brachycera)}

\format{
  A data frame with 192 observations on the following 15 variables.
  \describe{
    \item{\code{Date}}{a POSIXt variable, the time of counting the individuals in the eklektor trap}
    \item{\code{Treatment}}{a factor with 4 levels \code{A} \code{B} \code{Standard} \code{Novum},
    where \code{Novum} is the novel treatment of interest in safety assessment, and \code{Standard} is the nearest standard treatment which commonly accepted.
    \code{A} and \code{B} are two additional standard treatments.}
    \item{\code{Block}}{a numeric vector, specifying the eight blocks 1-8}
    \item{\code{Plot}}{a factor with levels \code{A1} \code{A2} to \code{Standard8}, indicator of the individuals plots}
    \item{\code{Agromy}}{a numeric vector, counts of individuals}
    \item{\code{Anthom}}{a numeric vector, counts of individuals}
    \item{\code{Callip}}{a numeric vector, counts of individuals}
    \item{\code{Chloro}}{a numeric vector, counts of individuals}
    \item{\code{Ephyd}}{a numeric vector, counts of individuals}
    \item{\code{Droso}}{a numeric vector, counts of individuals}
    \item{\code{Hybo}}{a numeric vector, counts of individuals}
    \item{\code{Musci}}{a numeric vector, counts of individuals}
    \item{\code{Phori}}{a numeric vector, counts of individuals}
    \item{\code{Sphaer}}{a numeric vector, counts of individuals}
    \item{\code{Total}}{a numeric vector, counts of individuals}
  }
}

\source{
...
}

\examples{
data(Brachycera)

par(mar=c(11,5,3,1))

boxplot(Total ~ Treatment*Date, data=Brachycera, las=2, 
col=c("white","white","blue","green"))

legend(x=15, y=80, legend=levels(Brachycera$Treatment),
 fill=c("white","white","blue","green"))

}
\keyword{datasets}
