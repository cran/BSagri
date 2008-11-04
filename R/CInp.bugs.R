`CInp.bugs` <-
function(x, conf.level=0.95, alternative="two.sided", whichp=NULL, ...)
{

args<-list(...)

sl<-x$sims.list

if(is.null(whichp))
{
mat<-x$sims.matrix
}
else{
 namsl<-names(sl)
 if(!whichp %in% namsl)
  {stop("whichp could not be found in the parameter list of the openbugs object")}

  if(length(whichp)==1)
   {
   mat<-sl[[whichp]]
   }
  if(length(whichp)>1)
   {
   mat<-matrix(nrow=x$n.sims)
   for (i in seq(along.with=whichp))
    {
     mat<-cbind(mat,x$sims.list[[whichp[i]]])
    }
   }
 }

args$x<-mat
args$conf.level<-conf.level
args$alternative<-alternative

out<-do.call("CInp.default", args)

return(out)
}

