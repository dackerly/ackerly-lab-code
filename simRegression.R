simRegression <- function(x,int=0,slp=1,rsd=1,show.plot=F,returny=F)
{
  y <- int + slp*x + rnorm(length(x),sd = rsd)
  fit <- lm(y~x)
  if (show.plot) {
    plot(y~x)
    abline(fit)
  }
  cfit <- coefficients(fit)
  mod.rsd <- sd(residuals(fit))
  pval <- coefficients(summary(fit))[2,4]
  results <- data.frame(N = length(x),true.int=int,true.slp=slp,true.rsd=rsd,mod.int=cfit[1],mod.slp=cfit[2],mod.rsd=mod.rsd,pval=pval)
  row.names(results) <- NULL
  if (returny) return(list(results,y)) else return(results)
}