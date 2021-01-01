# common components

partial_estimator <- function( X, coef, residuals )
{
  return(mean( X[,coef] * residuals ))
}

soft_threshold <- function( partial_estimate, lambda )
{
  if( partial_estimate < -lambda )
  {
    return( partial_estimate + lambda )
  }
  else if( partial_estimate > lambda )
  {
    return( partial_estimate - lambda )
  }# TODO: fix soft thresholding for constant term
  return(0)
}

firm_threshold <- function( partial_estimate, lambda, gamma )
{
  if( abs(partial_estimate) < lambda*gamma )
  {
    return( (gamma/(gamma-1))* soft_threshold(partial_estimate,lambda) )
  }
  return(partial_estimate)
}

standardizer <- function( x )
{
  if(is.vector(x) && is.numeric(x))
  {
    x_mean <- mean(x)
    sd_x <- sd(x)
    x <- (x-mean_x)/sd_x
  }
  else if( is.vector(x) && is.character(x) || is.factor(x) )
  {
    y <-   as.integer(as.factor(y) )
  }
  else if( is.matrix(x) )
  {
    x <- as.data.frame(x)
    mean_x <- sapply(x, mean)
    sd_x <- sapply(x, sd )
    colnames_x <- colnames(x)

    x <- as.data.frame(lapply(1:ncol(x), function(i){
      return( (x[,i] - mean_x[i]) / sd_x[i])
    }))
    colnames(x) <- colnames_x
  }
  return( list( x_scaled = as.matrix(x), mean_x = mean_x, sd_x = sd_x ) )
}
