\name{Nematocera}
\alias{Nematocera}
\docType{data}
\title{ Trap counts of Nematocera}
\description{
  In a field trial, 4 treatments (A,B, Standard, Novum) were arranged in a randomized complete block design with 8 blocks
 and 32 plots. In summer 2005 soil eklektor traps were placed in each plot, on six dates from
 2005-07-12 to 2005-09-25, the number of individuals of Nematocera (gnats, midges and others)
 hatching from soil were counted. The individuals were classified to the family level.
 Interest was in assessing potential effects of a novel agricultural practice (Novum)
 on the abundance of Nematocera.
}
\usage{data(Nematocera)}
\format{
  A data frame with 192 observations on the following 14 variables.
  \describe{
    \item{\code{Date}}{a POSIXt, the time of counting the individuals in the eklektor trap}
    \item{\code{Treatment}}{a factor with 4 levels, \code{A}, \code{B}, \code{Standard} and \code{Novum}, where \code{Novum} is the novel treatment, \code{Standard} is the standard treatment most similar to \code{Novum}, and \code{A} and \code{B} are additional standard treatments.}
    \item{\code{Block}}{a numeric vector, specifying the blocks 1-8}
    \item{\code{Plot}}{a factor with 32 levels \code{A1} to \code{Standard8}, indicator variables for the individual eklektors}
    \item{\code{Bibio}}{a numeric vector, counts of individuals, belonging to the family}
    \item{\code{Cecido}}{a numeric vector, counts of individuals, belonging to the family}
    \item{\code{Cerato}}{a numeric vector, counts of individuals, belonging to the family}
    \item{\code{Chiro}}{a numeric vector, counts of individuals, belonging to the family}
    \item{\code{Myceto}}{a numeric vector, counts of individuals, belonging to the family}
    \item{\code{Psycho}}{a numeric vector, counts of individuals, belonging to the family}
    \item{\code{Scato}}{a numeric vector, counts of individuals, belonging to the family}
    \item{\code{Sciari}}{a numeric vector, counts of individuals, belonging to the family}
    \item{\code{Tipuli}}{a numeric vector, counts of individuals, belonging to the family}
    \item{\code{Total}}{a numeric vector, total count of individuals belonging to the suborder Nematocera}
  }
}

\source{
personal communications, S.Prescher, JKI Braunschweig, Germany
}

\examples{
data(Nematocera)

par(mar=c(11,5,3,1))

boxplot(Total ~ Treatment*Date, data=Nematocera, las=2, col=c("white","white","blue","green"))
legend(x=15, y=100, legend=levels(Nematocera$Treatment), fill=c("white","white","blue","green"))

pairs(Nematocera[,c("Cecido","Cerato","Chiro","Myceto","Psycho","Sciari")])

}
\keyword{datasets}
