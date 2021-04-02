coordinate_cycle_elnet <- function( y, X, beta, weights, lambda = 0.5, alpha = 0.5  )
{
  partial_residual <- y - X%*%beta
  for(i in 1:ncol(X))
  {
    partial_estimate <- partial_estimator( X, coef = i, residuals = partial_residual ) + beta[i]
    update <- soft_threshold( partial_estimate,
                              weights[i]*lambda*alpha)/(1+(lambda*weights[i]*(1-alpha)))
    partial_residual <- partial_residual - (update - beta[i])*X[,i]
    beta[i] <- update
  }
  return(beta)
}

elasticnet_regression <- function( y, X, weights = NULL, lambda = 0.7,alpha = 0.5, tolerance = 1e-12, max_iterations = 200,
                                   standardize = TRUE, intercept = TRUE )
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

  if( intercept )
  {
    a <- mean(y)
  }

  coef <- list()
  coef[[1]] <- runif(ncol(X_))

  if(is.null(weights))
  {
    weights <- rep(1,ncol(X_))
  }

  weights <- weights/sum(weights)*length(weights)

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
      coef[[iter]] <- coordinate_cycle_elnet(y,
                                                X_,
                                             weights = weights,
                                                beta = coef[[iter-1]],
                                                lambda = lambda_val,
                                                alpha = alpha)
      if( all( abs(coef[[iter]] - coef[[iter-1]]) < tolerance ) ){break()}
    }

    result[[as.character(lambda_val)]] <- coef[[length(coef)]]
  }

  if(standardize)
  {
    result <- lapply( result, FUN = function(i){
      coef <- unstandardizer( i, X )
      if( intercept )
      {
        a <- a - sum( coef * X[["mean_x"]] )
        coef <- c(a, coef)
      }
      return( coef )
    })
  }

  names(result) <- lambda
  return(result)
}

