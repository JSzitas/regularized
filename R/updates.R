
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
