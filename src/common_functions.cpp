#include <Rcpp.h>
#include <RcppEigen.h>
using namespace Rcpp;
using namespace RcppEigen;
using namespace Eigen;


// [[Rcpp::export]]
double partial_estimator( const Eigen::VectorXf & x, const Eigen::VectorXf & residuals  )
{
  return (x * residuals).mean();
}

// [[Rcpp::export]]
double soft_threshold( const double & partial_estimate, const double & lambda )
{
  if( partial_estimate < -lambda )
  {
    return partial_estimate + lambda;
  }
  else if( partial_estimate > lambda )
  {
    return partial_estimate - lambda;
  }
  return 0;
}
// [[Rcpp::export]]
double firm_threshold( const double & partial_estimate, const double & lambda, const double & gamma )
{
  if( abs(partial_estimate) < lambda*gamma )
  {
    return (gamma/(gamma-1))* soft_threshold(partial_estimate,lambda);
  }
  return partial_estimate;
}
// [[Rcpp::export]]
double scad_threshold( const double & partial_estimate, const double & lambda, const double & gamma)
{
  if(abs(partial_estimate) < 2*lambda)
  {
    return soft_threshold(partial_estimate, lambda);
  }
  else if( abs(partial_estimate) > lambda*gamma)
  {
    return partial_estimate;
  }
  return soft_threshold(partial_estimate,lambda) * ((gamma-1)/(gamma-2));
}
