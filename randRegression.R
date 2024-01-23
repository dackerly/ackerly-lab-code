randRegression <- function(x,y,show.plot=F)
{
  yr <- sample(y,length(y),replace=F)
  if (show.plot) plot(yr~x)
  fit <- lm(yr~x)
  return(coefficients(fit)[2])
}