
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


# ridge regression without gradient descent

ridge_regression <- function( y, X, lambda = 1,
                              standardize = TRUE,
                              include_intercept = TRUE)
{
  if(standardize)
  {
    X <- standardizer( X )
    X_ <- X[["x_scaled"]]
  }
  else
  {
    X_ <- X
  }
  if(include_intercept)
  {
    X_ <- cbind(rep(1,nrow(X_)), X_)
  }

  coef <- solve( t(X_) %*% X_ + diag(lambda, nrow = ncol(X_)) )%*%t(X_)%*%y

  # pred <- X_ %*% coef
  return(coef)
}

calculate_gradient <- function( y, X, weights )
{
  -2 * t(X) %*% (y - X %*% weights)
}

ridge_regression_gradient <- function(y,
                                      X,
                                      eta = 0.00005,
                                      lambda = 0.7,
                                      tolerance = 1e-9,
                                      max_iterations = 100,
                                      standardize = TRUE,
                                      include_intercept = TRUE)
{

  if(standardize)
  {
    X <- standardizer(X)
    X_ <- X[["x_scaled"]]
  }
  else{
    X_ <- X
  }
  if(include_intercept)
  {
    X_ <- cbind(rep(1,nrow(X_)), X_)
  }

  weights <- runif( ncol(X_) )
  grad <- calculate_gradient( y, X_, weights )

  for( iter in 1:max_iterations )
  {
    grad <- calculate_gradient( y, X_, weights )
    update <- weights - eta*( grad + 2*lambda*weights )
    if( all( (abs(weights - update)) < tolerance )){ break() }
    weights <- update
  }
  return(weights)
}


# sampler_descent <- function(y,
#                             X,
#                             alpha = 0.02,
#                             lambda = 0.7,
#                             tolerance = 0.03,
#                             max_samples = 10000)
# {
#
#   weights <- runif( ncol(X) )
#
#   for( iter in 1:max_iterations )
#   {
#     grad <- calculate_gradient( y, X, weights )
#     update <- weights - alpha * grad
#     if( all( (abs(weights - update)) > tolerance )) break();
#     weights <- update
#   }
#   return(weights)
# }

# candidates <- function(  )

c(X1 = 0.335207734859876, X2 = 0.970408360698419, X3 = 0.478708368726153,
  X4 = 0.0141181556126571, X5 = 0.0408025853092904, X6 = -0.000228685831025933,
  X7 = 0.00233202503139908, X8 = 0.00793453526339912, X9 = 0.0172621407261373,
  X10 = -0.0298367239592512)




