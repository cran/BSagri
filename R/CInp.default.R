`CInp.default` <-
function(x, conf.level=0.95, alternative="two.sided", ...)
{
alternative <- match.arg(alternative, choices=c("two.sided","less","greater"))

args<-list(...)

DataMatrix <- x
N <- nrow(DataMatrix)
k <- round(conf.level*N,0)

switch(alternative,

"two.sided"={
probs<-c((1-conf.level)/2, 1-(1-conf.level)/2)
CIs <- t( apply( X=DataMatrix, MARGIN=2, 
 FUN=function(x){quantile(x=x, probs=probs)} ))
},

"less"={
probs<-c(conf.level)
upper <- t( apply( X=DataMatrix, MARGIN=2, 
 FUN=function(x){quantile(x=x, probs=probs)} ))
CIs<-cbind(-Inf, upper)
},

"greater"={
probs<-c(1-conf.level)
lower <- t( apply( X=DataMatrix, MARGIN=2, 
 FUN=function(x){quantile(x=x, probs=probs)} ))
CIs<-cbind(lower,Inf)
}
)
# end of switch

estimate <- apply(X=DataMatrix, MARGIN=2, median)

colnames(CIs)<-c("lower","upper")

out<-list(
conf.int=CIs,
estimate=estimate,
x=x,
k=k,
N=N,
conf.level=conf.level,
alternative=alternative)

class(out)<-"CInp"

return(out)

}

