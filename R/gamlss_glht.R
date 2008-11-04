"vcov.gamlss" <- function
(object,...)

{
  Qr <- object$mu.qr
  p <- object$mu.df
  p1 <- 1:(p - object$mu.nl.df)
  chol2inv(Qr$qr[p1, p1, drop = FALSE])
}

"modelparm.gamlss" <- function
(model, coef. = coef, vcov. = vcov.gamlss, df = NULL, ...)
 {multcomp:::modelparm.default(model, coef. = coef., vcov. = vcov., df = df, ...)}
    
    


