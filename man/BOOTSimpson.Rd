\name{BOOTSimpson}
\alias{BOOTSimpsonD}
\alias{BOOTSimpsonR}

\title{ Simultaneous confidence intervals for Simpson indices }
\description{
NOTE: This is a Test-version and is not sufficiently checked for correctness so far.
Simultaneous confidence intervals for differences and ratios of Simpsons indices of diversity
are calculated from data sets with repeated samples of communities and designs with 
more than two treatments groups. The intervals are calculated based on a stratified, non-parametric 
ordinary bootstrap sample of Simpsonindices, and applying the Algorithm of Besag et al.(1995) on the joint
empirical distribution of differences (BOOTSimpsonD) or ratios (BOOTSimpsonR) of the original distribution.
}
\usage{
BOOTSimpsonD(X, f, type = "Dunnett", cmat = NULL, conf.level = 0.95,
 alternative=c("two.sided", "less", "greater"), madj=TRUE, ...)

BOOTSimpsonR(X, f, type = "Dunnett", cmat = NULL, conf.level = 0.95,
 alternative=c("two.sided", "less", "greater"), madj=TRUE, ...)

}

\arguments{
  \item{X}{ a data.frame of dimension n times p, containing integer entries as species counts of p species from n independent samplings}
  \item{f}{ a factor, usually with more than two levels. Must be of length n, when X is an n times p matrix }
  \item{type}{ a single character string, defining a contrast type. Supported options are 'Dunnett','Tukey','Sequen'; for more options, see \code{?contrMatratio} in \pkg{mratios} when using \code{BOOTSimpsonR}
or \code{contrMat} in \pkg{multcomp} when using \code{BOOTSimpsonD} }
  \item{cmat}{ user defined contrasts:
 when using \code{BOOTSimpsonD}, this should be a numeric contrast matrix, with k columns when the factor \code{f} has k levels and should define differences of these levels;
 when using \code{BOOTSimpsonR}, this should be a list with two numeric contrast matrices in the entries \code{numC} and \code{denC} for numerators and denominator of the ratios, respectively; each having k columns when the factor \code{f} has k levels
 }
  \item{conf.level}{  a single numeric value between 0.5 and 1  }
  \item{alternative}{ a single character string, one of 'two.sided','less' and 'greater' }
  \item{madj}{ a single logical value, indicating whether simultaneous (if \code{TRUE}, the default) or marginal confidence intervals (if FALSE) shall be computed }
  \item{\dots}{ Further arguments to be passed to the function \code{boot} from package \pkg{boot};
 most importantly, the number of Bootstrap samples can be chosen via the parameter \code{R} (default is \code{R=999}); see \code{?boot} for further options.
 }
}
\details{
  NOTE: This is a test version!
}
\value{
  If \code{madj=TRUE}, an object of class "SCSnp", see \code{\link{SCSnp}} for details.
  If \code{madj=FALSE}, an object of class "CInp", see \code{\link{CInp}} for details.
}

\author{ Frank Schaarschmidt }

\seealso{ \code{\link{SCSnp}},
 these function internally make use of \code{\link{CCDiff.boot}}, \code{\link{CCDiff.default}},
 \code{\link{CCRatio.boot}}, \code{\link{CCRatio.default}},
 \code{boot} and \code{estSimpsonf}.
 }
\examples{

X<-t(rmultinom(n=40,size=100,
 prob=c(0.3,0.2,0.2,0.1,0.1,0.05,0.05)))

colnames(X)<-paste("Sp",1:7,sep="")

DAT<-as.data.frame(X)

f<-as.factor(rep(c("A","B","C","D"),each=10))

SCIdunnettd<-BOOTSimpsonD(X=DAT, f=f, type = "Dunnett",
 conf.level = 0.95, alternative = "two.sided")

SCIdunnettd

SCIdunnettr<-BOOTSimpsonR(X=DAT, f=f, type = "Dunnett",
 conf.level = 0.95, alternative = "two.sided")

SCIdunnettr

}

\keyword{ htest }
\concept{ simultaneous confidence }
\concept{ bootstrap }
\concept{ diversity }
