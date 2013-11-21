\name{Diptera}
\alias{Diptera}
\docType{data}
\title{  Soil eklektor data for some families of Diptera}
\description{
  Hatching of some families of Diptera was recorded in summer 2005 using eklektors covering 2 square meters
 of soil surface each. A total of 32 eklektors were arranged in a randomized field trial. Total counts of
 individuals over the whole season are reported. Aim was to assess the impact of a novel treatment on the abundance of
 Diptera with larval development in the soil, compared to three standard treatments.
}
\usage{data(Diptera)}
\format{
  A data frame with 32 observations on the following 7 variables.
  \describe{
    \item{\code{Callip}}{a numeric vector}
    \item{\code{Chloro}}{a numeric vector}
    \item{\code{Ephyd}}{a numeric vector}
    \item{\code{Droso}}{a numeric vector}
    \item{\code{Ges}}{a numeric vector, total number of species}
    \item{\code{Chiro}}{a numeric vector}
    \item{\code{Treatment}}{a factor, specifying the four different treatments,
 with levels \code{S1} \code{S2} for two standard treatments, \code{SNovum} for the standard treatment most similar to the novel treatment, and \code{Novum}, for the novel treatment}
  }
}

\source{
personal communications S. Prescher, JKI Braunschweig, Germany
}

\examples{
data(Diptera)


layout(matrix(1:6, nrow=3))

boxplot(Callip~Treatment, data=Diptera, horizontal=TRUE, las=1,
 main="Abundanz Callip", col=c("white","white","blue","red"))

boxplot(Chloro~Treatment, data=Diptera, horizontal=TRUE, las=1,
 main="Abundanz Chloro", col=c("white","white","blue","red"))

boxplot(Ephyd~Treatment, data=Diptera, horizontal=TRUE, las=1,
 main="Abundanz Ephyd", col=c("white","white","blue","red"))

boxplot(Droso~Treatment, data=Diptera, horizontal=TRUE, las=1,
 main="Abundanz Droso", col=c("white","white","blue","red"))

boxplot(Chiro~Treatment, data=Diptera, horizontal=TRUE, las=1,
 main="Abundanz Chiro", col=c("white","white","blue","red"))

boxplot(Ges~Treatment, data=Diptera, horizontal=TRUE, las=1,
 main="Abundanz all Diptera", col=c("white","white","blue","red"))


}

\keyword{datasets}
