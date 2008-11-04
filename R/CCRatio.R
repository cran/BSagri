`CCRatio` <-
function(bugs, dat, cmat=NULL,
 type=c("Dunnett", "Tukey", "Sequen", "Williams", "Changepoint"))
{
require(mratios)

type<-match.arg(type)

if(class(bugs)!="bugs")
 {stop("argument bugs must be an object of class 'bugs'")}

if(class(dat)!="R2Bugsdat1w")
 {stop("argument dat must be an object of class 'R2Bugsdat1w'")}

if(dat$Intercept==TRUE)
 {stop("dat$Intercept must be FALSE")}


ngroup<-dat$names$ni

chains<-bugs$sims.list$muvec


if(is.null(cmat))
{
cmat<-contrMatRatio(n=ngroup, type=type)
}
else
{
if(!is.list(cmat))
 {stop("cmat must be a list")}

if(is.null(cmat$numC)|is.null(cmat$denC))
 {stop("cmat must be a list with elements $numC and $denC, specifying the numerator and denominator contrast coefficients")}

if(!is.matrix(cmat$numC)|!is.matrix(cmat$denC))
 {stop("elements $numC and $denC of 'cmat' must be matrices, specifying the numerator and denominator contrast coefficients")}

if(ngroup!=ncol(cmat$numC))
 {stop("ncol(cmat$numC) must be the same as the number of means in muvec")}

if(ngroup!=ncol(cmat$denC))
 {stop("ncol(cmat$denC) must be the same as the number of means in muvec")}

}

nchains<-apply(X=chains, MARGIN=1, FUN=function(x){(cmat$numC%*%x) / (cmat$denC%*%x)})

if(nrow(cmat$numC)==1)
 {nchains<-matrix(nchains, nrow=1)}

rownames(nchains)<-rownames(cmat$numC)

out<-list(
chains=t(nchains),
bugs=bugs,
dat=dat,
cmat=cmat
)

class(out)<-"CCRatio"

return(out)

}

