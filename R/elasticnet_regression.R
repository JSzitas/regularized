coordinate_cycle_elnet <- function( y, X, beta, lambda = 0.5, alpha = 0.5  )
{
  partial_residual <- y - X%*%beta
  for(i in 1:ncol(X))
  {
    partial_estimate <- partial_estimator( X, coef = i, residuals = partial_residual ) + beta[i]
    update <- soft_threshold( partial_estimate( y, X, coef = i, beta ),
                              lambda*alpha)/(1+(lambda*(1-alpha)))
    partial_residual <- partial_residual - (update - beta[i])*X[,i]
    beta[i] <- update
  }
  return(beta)
}

elasticnet_regression <- function( y, X, lambda = 0.7,alpha = 0.5, tolerance = 1e-9, max_iterations = 200,
                              standardize = TRUE )
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
  # TODO: standardize Y and include the estimated mean as intercept

  weights <- list()
  weights[[1]] <- runif(ncol(X_))

  lambda <- sort(lambda, decreasing = TRUE)

  result <- list()
  # Solve for the full solution path - this is in someway simpler than
  # solving just for a value in the middle, as we can easily see how coefficients
  # contiunously shrink with decreasing lambda values - it therefore makes sense
  # to reuse them.
  for( lambda_val in lambda )
  {
    for( iter in 2:max_iterations )
    {
      weights[[iter]] <- coordinate_cycle_elnet(y,
                                          X_,
                                          beta = weights[[iter-1]],
                                          lambda = lambda_val,
                                          alpha = alpha)
      if( all( abs(weights[[iter]] - weights[[iter-1]]) < tolerance ) ){break()}
    }
    result[[as.character(lambda_val)]] <- weights[[length(weights)]]
    weights[[1]] <- weights[[length(weights)]]
  }
  names(result) <- lambda


  return(result)
}
