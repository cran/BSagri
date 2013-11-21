\name{Feeding}
\alias{Feeding}
\docType{data}
\title{ Pupation and Hatching rate in a feeding experiment with four varieties}
\description{
  Larvae of a non-target organism were fed with plant material derived from a novel variety(Novum),
 material from three standard varieties (NStandard: the standard variety most similar to Novum,
 and two additional standard varieties S1 and S2). Objective was to assess the impact of Novum on the pupation and hatching rate of an animal that potentially feeds on plant material compared to accepted standard varieties.  

}
\usage{data(Feeding)}
\format{
  A data frame with 32 observations on the following 5 variables.
  \describe{
    \item{\code{Rep}}{a factor with 32 levels indexing the 32 replications} 
    \item{\code{Variety}}{a factor with 4 levels: \code{S1} and \code{S2} are two standard varieties, \code{Novum} is a novel variety, and \code{NStandard} is the standard variety most similar to \code{Novum} }
    \item{\code{Total}}{the total number of animals in each experimental unit}
    \item{\code{Pupating}}{number of individuals pupating in each unit, the others died}
    \item{\code{Hatching}}{number of individuals hatching from the pupae}
  }
}


\examples{
data(Feeding)

# Larval mortality:

Feeding$Lmort <- Feeding$Total - Feeding$Pupating

fit1<-glm(cbind(Pupating,Lmort)~Variety,data=Feeding, family=quasibinomial)
anova(fit1, test="F")


}
\keyword{datasets}
