`CCDiff` <-
function(bugs, dat, cmat=NULL,
 type=c("Dunnett", "Tukey", "Sequen", "Williams", "Changepoint"))
{
require(multcomp)

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
cmat<-contrMat(n=ngroup,type=type)
}
else{

if(!is.matrix(cmat))
 {stop("'cmat' must be a matrix, specifying the contrast coefficients")}

if(ngroup!=ncol(cmat))
 {stop("ncol(cmat) must be the same as the number of means in muvec")}

cs<-apply(cmat,1,sum)

if(any(cs!=0))
 {warning("Rows of cmat do not sum up to zero. Are the contrasts appropriately defined?")}

}

nchains<-apply(X=chains, MARGIN=1, FUN=function(x){cmat %*% x})

if(nrow(cmat)==1)
 {nchains<-matrix(nchains, nrow=1)}

rownames(nchains)<-rownames(cmat)

out<-list(
chains=t(nchains),
bugs=bugs,
dat=dat,
cmat=cmat,
)

class(out)<-"CCDiff"
return(out)

}

