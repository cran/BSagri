"vcov.geeglm" <- function
(object,...)

{
sobj<-summary(object)

"df.geeglm"<-function(object)
{
object$df.residual
}


return(sobj$cov.scaled)
}

"modelparm.geeglm" <- function
(model, coef. = coef, vcov. = vcov.geeglm, df = NULL , ...)
 {multcomp:::modelparm.default(model, coef. = coef., vcov. = vcov., df = df., ...)}
    
    