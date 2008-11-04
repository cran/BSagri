\name{Cica1}
\alias{Cica1}
\docType{data}
\title{ Catches of Planthoppers and Leafhoppers }
\description{
 Data of a field trial concerning the impact of a genetically modified variety on the abundance of Planthoppers and Leafhoppers.
The trial was designed as a randomized complete block design with 8 blocks (Row). In each block, three treatments
 were randomized: a conventional variety treated with insecticides (Insecticide), a genetically modified variety (GM),
 and the near-isogenic line (Iso) the to genetically modified line.
}
\usage{data(Cica1)}
\format{
  A data frame with 24 observations on the following 6 variables.
  \describe{
    \item{\code{Field}}{a factor with levels \code{1} \code{2}, separating the two major sites of the trial. On field 1, the blocks 1-5 were situated, on field 2, blocks 6-8 were situated. }
    \item{\code{Row}}{a factor with 8 levels, specifying the blocks:\code{R1} \code{R2} \code{R3} \code{R4} \code{R5} \code{R6} \code{R7} \code{R8}}
    \item{\code{Year}}{a numeric vector, for year 1 of the trial}
    \item{\code{Treatment}}{a factor with 3 levels, specifying the genetically modified variety \code{GM},
 the conventional variety treated with insecticides \code{Insecticide},
 and the variety that was near-isogenic to the GM variety \code{Iso}}
    \item{\code{Au_Bonitur}}{Counts of Auchenorryhncha by visual assessment}
    \item{\code{Zs_sweep_netting}}{Counts of the major species Zyginidia scutellaris, catched by sweep nets}
  }
}

\source{
...
}

\examples{
data(Cica1)

layout(matrix(1:2,ncol=1))

ylim<-range(Cica1[,c("Au_Bonitur","Zs_sweep_netting")])

boxplot(Au_Bonitur ~ Treatment, data=Cica1,
 main= "Aucherrhyncha, visual assessment", ylim=ylim)

boxplot(Zs_sweep_netting ~ Treatment, data=Cica1,
 main="Z.scutellaris, sweep netting", ylim=ylim)


}
\keyword{datasets}
