`CCRatio.default` <-
function(x, cmat)
{
require(mratios)

ngroup<-ncol(x)

chains<-x

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

nchains<-apply(X=chains, MARGIN=1, FUN=function(x){(cmat$numC%*%x) / (cmat$denC%*%x)})

if(nrow(cmat$numC)==1)
 {nchains<-matrix(nchains, nrow=1)}

rownames(nchains)<-rownames(cmat$numC)

out<-list(
chains=t(nchains),
x=x,
cmat=cmat
)

class(out)<-"CCRatio"

return(out)

}

