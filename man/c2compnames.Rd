\name{c2compnames}
\alias{c2compnames}

\title{ Define row names of a contrast matrix, depending on its column names}
\description{
Define row names of a contrast matrix, depending on its column names, as can be necessary for contrasts matrices.
Currently, two options to do that are available.
}
\usage{
c2compnames(cmat, ntype = "aggr")
}

\arguments{
  \item{cmat}{ a contrast matrix }
  \item{ntype}{ a single character string,
 defining how to build names from the column names of \code{cmat},
 currently onls \code{"aggr"}
 (aggregates names of groups with negative and positive coefficients),
 and \code{"sequ"}
 }
}


\value{
The input matrix cmat, with its row names replaced.
}


\seealso{ \code{contrMat} in \pkg{multcomp} to define contrast matrices of different types}
\examples{


# names for interaction contrasts:

n1<-c(10,10,10,10)
names(n1)<-c("A","B","C","D")

n2<-c(3,3,3)
names(n2)<-c(1,2,3)

library(multcomp)

CMT1<-contrMat(n1, type="Tukey")

CMT2<-contrMat(n2, type="Tukey")

IAC<-IAcontrastsCMAT(CMAT1=CMT1, CMAT2=CMT2)

c2compnames(IAC, ntype="aggr")

c2compnames(IAC, ntype="sequ")


###############################

# names for Williams-type contrasts:

n1<-c(10,10,10,10)
names(n1)<-c("C0","D1","D5","D10")


CMW<-contrMat(n1, type="Williams")

CMW

c2compnames(CMW, ntype="aggr")

c2compnames(CMW, ntype="sequ")



}

\keyword{ misc }

