`CCDiff.boot` <-
function(x, cmat=NULL,
 type=c("Dunnett","Tukey","Sequen","Williams","Changepoint","McDermott","GrandMean","Marcus"))
{
require(multcomp)

type<-match.arg(type)

if(type %in% c("Williams","Changepoint","McDermott","Marcus","GrandMean"))
 {warning("This is a test version. Choosing contrasts types differing from 'Dunnett','Tukey' or 'Sequen' might make no sense in case of unbalanced designs!")}

ngroup<-ncol(x$t)

f<-x$strata

ni<-unlist(lapply(split(f,f=f),length))

gnames<-names(x$t0)

names(ni)<-gnames

if(any(ni<5))
 {warning("For sample sizes les than 5 this function hardly makes sense!")}


if(is.null(cmat))
{
cmat<-contrMat(n=ni,type=type)
}

chains <- x$t

out<-CCDiff.default(x=chains, cmat=cmat)

return(out)

}

