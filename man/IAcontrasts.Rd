\name{IAcontrasts}
\alias{IAcontrasts}

\title{ Interaction contrasts for 2-factorial designs }
\description{
  Builds a family of intercation contrasts for complete two-factorial designs.
}
\usage{
IAcontrasts(type, k)
}

\arguments{
  \item{type}{ a vector of two character strings, specifying the contrast \code{type}, currently, \code{"Tukey"}, \code{"Dunnett"}, \code{"Sequence"} or \code{"Identity"} are accepted }
  \item{k}{ a vector of two integers, specifying the number of groups in each factor of the two-factorial design }
}
\details{
  Creates contrast matrices using \code{contrMat} from package \pkg{multcomp}, creates the kronecker product of both and creates suitable columnnames.
}
\value{
  A matrix with k[1]*k[2] columns and a number of rows depending on type.

}


\seealso{ for 2-way interaction contrasts directly based on 2 contrasts matrices, see \code{\link{IAcontrastsCMAT}};
two possibilities to specify appropriate rownames are implemented in function \code{\link{c2compnames}} }

\examples{

IAC<-IAcontrasts(type=c("Tukey", "Tukey"), k=c(3,4))

IAC

IAC2<-c2compnames(IAC, ntype="sequ")
IAC2

}

\keyword{ htest }
\concept{ multiple comparisons }
\concept{ interaction }
