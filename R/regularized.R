coordinate_cycle <- function( y, X, penalty = "elasticnet", link_type = "logit", beta, weights, lambda = 0.5, alpha = 0.5, gamma = 2.7  )
{
  partial_residual <- y - link( X%*%beta, link_type = link_type )
  for(i in 1:ncol(X))
  {
    partial_estimate <- partial_estimator( X, coef = i, residuals = partial_residual ) + beta[i]
    update <- update_coef( coef = partial_estimate, penalty, lambda, alpha, weights[i], gamma )
    partial_residual <- partial_residual - (update - beta[i])*X[,i]
    beta[i] <- update
  }
  return(beta)
}

update_coef <- function( coef, penalty = c("elasticnet","mcp","scad"), lambda = 0.5, alpha = 0.5, weight = 1, gamma = 2.5 )
{
  fun <- list( elasticnet = elasticnet_update,
               mcp = mcp_update,
               scad = scad_update )[[penalty]]
  do.call( fun,
           list( coef = coef, lambda = lambda, alpha = alpha, weight = weight, gamma = gamma ))
}

elasticnet_update <- function( coef, lambda, alpha, weight, gamma )
{
  return( soft_threshold( coef, weight * lambda * alpha )/( 1 + ( lambda * weight * ( 1 - alpha ))) )
}

scad_update <- function( coef, lambda, alpha, weight, gamma )
{
  return( scad_threshold( coef, weight * lambda, gamma) )
}

mcp_update <- function( coef, lambda, gamma = NULL, alpha = NULL, weight = NULL )
{
  return( firm_threshold( coef, lambda * weight, gamma ) )
}


regularized <- function( y,
                         X,
                         penalty = c("elasticnet","mcp","scad"),
                         link = c("identity","logit"),
                         weights = NULL,
                         lambda = 0.7,
                         alpha = 0.5,
                         gamma = 2.5,
                         tolerance = 1e-12,
                         max_iterations = 200,
                         standardize = TRUE,
                         intercept = TRUE )
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
  # initialize coefficients with random numbers
  coef[[1]] <- runif(ncol(X_))
  # if individual penalties were not supplied, set weights to 1
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
      coef[[iter]] <- coordinate_cycle( y,
                                        X_,
                                        penalty = penalty,
                                        weights = weights,
                                        link_type = link,
                                        beta = coef[[iter-1]],
                                        lambda = lambda_val,
                                        alpha = alpha,
                                        gamma = gamma )
      if( all( abs(coef[[iter]] - coef[[iter-1]]) < tolerance ) ){break()}
    }

    result[[as.character(lambda_val)]] <- coef[[length(coef)]]
  }
  # if we standardized the coefficients, we have some fixing to do
  if(standardize)
  {
    result <- lapply( result, FUN = function(i){
      coef <- unstandardizer( i, X )
      # similarly, we have to do a bit of fixing on the intercept term
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
