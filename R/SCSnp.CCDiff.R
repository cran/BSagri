`SCSnp.CCDiff` <-
function(x,...)
{
args<-list(...)

args$x<-x$chains

out<-do.call("SCSnp.default", args)

out$x<-x

return(out)

}

