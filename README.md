
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regularized

<!-- badges: start -->

[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/JSzitas/regularized?branch=master&svg=true)](https://ci.appveyor.com/project/JSzitas/regularized)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.com/JSzitas/regularized.svg?branch=master)](https://travis-ci.com/JSzitas/regularized)
[![Codecov test
coverage](https://codecov.io/gh/JSzitas/regularized/branch/master/graph/badge.svg)](https://codecov.io/gh/JSzitas/regularized?branch=master)
[![R build
status](https://github.com/JSzitas/regularized/workflows/R-CMD-check/badge.svg)](https://github.com/JSzitas/regularized/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/regularized)](https://CRAN.R-project.org/package=regularized)
<!-- badges: end -->

This repository is an attempt at a simple implementation of several
regularisation methods.

Notably these include lasso, elasticnet (of which lasso is technically a
special case), MCP (minimax concave penalty) and SCAD (smoothly clipped
absolute deviations).

Adaptive lasso and relaxed lasso will also be considered eventually.

Nothing here has been tested very thoroughly, and it might break easily.
