\name{Decomp}
\alias{Decomp}
\docType{data}
\title{ A simulated data set}
\description{
 A simulated data set, resembling an experiment on the decomposition
 of plant material from four different varieties in soil.
}
\usage{data(Decomp)}
\format{
  A data frame with 1152 observations on the following 5 variables.
  \describe{
    \item{\code{Block}}{a numeric vector, giving the names of the Blocks}
    \item{\code{PID}}{a numeric vector, giving the the number of the experimental unit}
    \item{\code{Variety}}{a factor with levels \code{N} \code{S1} \code{S2} \code{S3}, specifying the variety assigned to the experimental unit, randomized within each Block}
    \item{\code{Time}}{a factor with levels \code{t1} \code{t2} \code{t3} \code{t4} \code{t5} \code{t6}, specifying time points at which measurements were taken}
    \item{\code{Perc}}{a numeric vector, giving the percentage of material}
  }
}
\details{
The experiment is a randomized complete block design, with repeated measurements within each experimental unit and additional subsamplings within each time point.
Plant material from four different varieties was deposited in bags in soil of 32 experimental units (coded by the variable PID), where the varieties had been grown in the vegetation period before. A total number of 36 bags was placed in each experimental unit.
 At six different time points, plant material was excavated and the content of the bags was analysed concerning the percentage of decomposition relative to the content at the begin of the experiment.
At each time point, six bags were excavated at each experimental unit. Some bags could not be found anymore (data missing).
The objective was to assess whether properties of the varieties obstruct the decompostion of plant material in the soil. The variety \code{N} is of special interest, while the varieties \code{S1}, \code{S2}, \code{S3} are standard varieties.  

Note, that this data set merely serves as an example to evaluate clustered data. Neither in the mean effects nor in the actual data points it does resemble a true experiment!
}
\source{
 Simulated.
}
\references{
 Resembling the experimental structure of an experiment performed by Sabine Prescher (JKI Braunschweig).
}
\examples{
data(Decomp)

}
\keyword{datasets}
