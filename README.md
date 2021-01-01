
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regularized

<!-- badges: start -->

<!-- badges: end -->

This repository contains basic implementations of several regularisation
methods. Currently they are only for regression (from a GLM point of
view the link function is identity). There is an implementation of
elasticnet (with ridge and lasso as special cases), and mcp.
Furthermore, the implementation tries to be fairly straight-forward and
demonstrative. Thus simple coordinate descent has been implemented,
without heavy optimisation (though those are intended in the future).
