`CInp.CCDiff` <-
function(x,...)
{
args<-list(...)

args$x<-x$chains

out<-do.call("CInp.default", args)

out$x<-x

return(out)

}

