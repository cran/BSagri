\name{Cica2}
\alias{Cica2}
\docType{data}
\title{  Catches of Planthoppers and Leafhoppers }
\description{
 Data of a field trial concerning the impact of a genetically modified variety on the abundance of Planthoppers and Leafhoppers.
The trial was designed as a randomized complete block design with 8 blocks (Row). In each block, three treatments
 were randomized: a conventional variety treated with insecticides (Insecticide), a genetically modified variety (GM),
 and the near-isogenic line (Iso) the to genetically modified line. These data originate from the second year of the trial in Cica1.
}
\usage{data(Cica2)}
\format{
  A data frame with 24 observations on the following 8 variables.
  \describe{
   \item{\code{Field}}{a factor with 2 levels, \code{1} \code{2}, separating the two major sites of the trial. On field 1, the blocks 1-5 were situated, on field 2, blocks 6-8 were situated.}
    \item{\code{Row}}{a factor with 8 levels, specifying the blocks: \code{R1} \code{R2} \code{R3} \code{R4} \code{R5} \code{R6} \code{R7} \code{R8}}
    \item{\code{Year}}{a numeric vector, with value 2 for the second year }
    \item{\code{Treatment}}{a factor with 3 levels, specifying the genetically modified variety \code{GM},
 the conventional variety treated with insecticides \code{Insecticide},
 and the variety that was near-isogenic to the GM variety \code{Iso}}
    \item{\code{Au_Bonitur}}{counts of Auchenorryhncha by visual assessment}
    \item{\code{Zs_sweep_netting}}{counts of the major species Zyginidia scutellaris, catched by sweep nets}
    \item{\code{Zs_yellow_traps}}{counts of Zyginidia scutellaris, catched by yellow traps}
    \item{\code{Zs_stick_traps}}{counts of Zyginidia scutellaris, catched by sticky traps}
  }
}

\source{
...
}
\references{
 See \code{\link{Cica1}} for data of the same trial a year earlier
}
\examples{
data(Cica2)


# A comparison of the treatments:

layout(matrix(1:4,ncol=4))

ylim<-range(Cica2[,c("Au_Bonitur","Zs_sweep_netting", "Zs_yellow_traps", "Zs_stick_traps")])

boxplot(Au_Bonitur ~ Treatment, data=Cica2,
 main= "Aucherrhyncha, visual assessment", ylim=ylim, horizontal=TRUE, las=1)

boxplot(Zs_sweep_netting ~ Treatment, data=Cica2,
 main="Z.scutellaris, sweep netting", ylim=ylim, horizontal=TRUE, las=1)

boxplot(Zs_yellow_traps ~ Treatment, data=Cica2,
 main="Z.scutellaris, yellow traps", ylim=ylim, horizontal=TRUE, las=1)

boxplot(Zs_stick_traps ~ Treatment, data=Cica2,
 main="Z.scutellaris, sticky traps", ylim=ylim, horizontal=TRUE, las=1)

# A comparison of sampling methods:

pairs(Cica2[,c("Au_Bonitur","Zs_sweep_netting", "Zs_yellow_traps", "Zs_stick_traps")])

}
\keyword{datasets}
