\name{Lepi}
\alias{Lepi}
\docType{data}
\title{ Insect counts of 12 Species}
\description{
 Simulated data, inpired by a real field investigating the potential impact of genetically modified crop
 on several insect species belonging to the same order. The trial was designed as a randomized complete block design with 8 blocks
 (Block), and a total of 24 plots. In each block, three treatments (Treatment) were randomized:
 a conventional variety treated with insecticides (Ins), a genetically modified variety (GM) without insecticide treatment,
 and the near-isogenic variety (Iso) the to genetically modified variety, without insecticide treatment.
 Individuals were counted (after classification to the species level) in two different dates in each year of the trial, where the the second date
 was of higher importance for assessment of impacts of GM variety on non-target species. In total 12 Species were observed during the trial.

}
\usage{data(Lepi)}
\format{
  A data frame with 144 observations on the following 17 variables.
  \describe{
    \item{\code{Year}}{a numeric vector, the year 1, 2, 3}
    \item{\code{Date}}{a numeric vector, 1 and 2 separating the 2 sampling date in each year }
    \item{\code{Block}}{a numeric vector, with values 1-8, indicator variable for the 8 blocks}
    \item{\code{Treatment}}{a factor with three levels identifying the varieties: \code{GM} is the genetically modified variety,
 \code{Ins} the conventional variety with insecticide treatment and \code{Iso} the near isognic line without insecticide treatment}
    \item{\code{Plot}}{a factor with 24 levels, identifying the individual plots}
    \item{\code{Sp1}}{counts of taxon 1}
    \item{\code{Sp2}}{counts of taxon 2}
    \item{\code{Sp3}}{counts of taxon 3}
    \item{\code{Sp4}}{counts of taxon 4}
    \item{\code{Sp5}}{counts of taxon 5}
    \item{\code{Sp6}}{counts of taxon 6}
    \item{\code{Sp7}}{counts of taxon 7}
    \item{\code{Sp8}}{counts of taxon 8}
    \item{\code{Sp9}}{counts of taxon 9}
    \item{\code{Sp10}}{counts of taxon 10}
    \item{\code{Sp11}}{counts of taxon 11}
    \item{\code{Sp12}}{counts of taxon 12}
  }
}

\source{
Simulated data.
}

\examples{

data(Lepi)

str(Lepi)

summary(Lepi)

SPEC<-names(Lepi)[-(1:5)]

# Occurrence

occur<-lapply(X=Lepi[,SPEC], FUN=function(x){length(which(x>0))})

unlist(occur)

# Species with reasonable occurence in the whole data:

SPEC2<-SPEC[c(1,2,3,6,8,9,11)]

pairs(Lepi[,SPEC2])

# 


layout(matrix(1:2, ncol=1 ))
par(mar=c(2,8,2,1))

boxplot(Sp2 ~ Treatment*Year, data=Lepi, main="Species 2",
 las=1, horizontal=TRUE, col=c("red","white","white"))

boxplot(Sp3 ~ Treatment*Year, data=Lepi, main="Species 3",
 las=1, horizontal=TRUE, col=c("red","white","white"))


}
\keyword{datasets}
