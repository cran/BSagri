\name{IAcontrastsCMAT}
\alias{IAcontrastsCMAT}

\title{ Interaction contrasts for a two-factorial design }
\description{
  Builds a family of intercation contrasts for complete two-factorial designs.
}
\usage{
IAcontrastsCMAT(CMAT1, CMAT2)
}
\arguments{
  \item{CMAT1}{ a (named) contrast matrix }
  \item{CMAT2}{ a (named) contrast matrix }
}
\details{
 Builds the kronecker product of \code{CMAT1} and \code{CMAT2} and creates suitable columnnames.
 Note that \code{CMAt1} and \code{CMAT2} are not checked, and hence its up to the user to define them suitably.
}
\value{
  A matrix with k[1]*k[2] columns.

}

\seealso{ for interaction contrasts based on contrast definition and the number of levels of the factors in atwo-way layout, see \code{\link{IAcontrasts}};
two possibilities to specify appropriate rownames are implemented in function \code{\link{c2compnames}} }

\examples{


library(multcomp)


n1<-c(10,10,10,10)
names(n1)<-c("A","B","C","D")

n2<-c(3,3,3)
names(n2)<-c(1,2,3)

CMT1<-contrMat(n1, type="Tukey")

CMT2<-contrMat(n2, type="Tukey")

IAC<-IAcontrastsCMAT(CMAT1=CMT1, CMAT2=CMT2)

c2compnames(IAC, ntype="sequ")

###


n1<-c(10,10,10,10)
names(n1)<-c("A","B","C","D")

n2<-c(3,3,3)
names(n2)<-c(1,2,3)

CMD1<-contrMat(n1, type="Dunnett")

CMD2<-contrMat(n2, type="Dunnett")

IAC<-IAcontrastsCMAT(CMAT1=CMD1, CMAT2=CMD2)

c2compnames(IAC, ntype="sequ")


}

\keyword{ htest }
