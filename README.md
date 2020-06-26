
<!-- README.md is generated from README.Rmd. Please edit that file -->

# probably

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/tidymodels/probably/branch/master/graph/badge.svg)](https://codecov.io/gh/tidymodels/probably?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/tidymodels/probably/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/probably/actions)
<!-- badges: end -->

## Introduction

probably contains tools to facilitate activities such as:

  - Conversion of probabilities to discrete class predictions.

  - Investigating and estimating optimal probability thresholds.

  - Inclusion of *equivocal zones* where the probabilities are too
    uncertain to report a prediction.

## Installation

You can install probably from CRAN with:

``` r
install.packages("probably")
```

You can install the development version of probably from GitHub with:

``` r
devtools::install_github("topepo/probably")
```

## Examples

Good places to look for examples of using probably are the vignettes.

  - `vignette("equivocal-zones", "probably")` discusses the new
    `class_pred` class that probably provides for working with equivocal
    zones.

  - `vignette("where-to-use", "probably")` discusses how probably fits
    in with the rest of the tidymodels ecosystem, and provides an
    example of optimizing class probability thresholds.

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

  - For questions and discussions about tidymodels packages, modeling,
    and machine learning, please [post on RStudio
    Community](https://rstd.io/tidymodels-community).

  - If you think you have encountered a bug, please [submit an
    issue](https://github.com/tidymodels/probably/issues).

  - Either way, learn how to create and share a
    [reprex](https://rstd.io/reprex) (a minimal, reproducible example),
    to clearly communicate about your code.

  - Check out further details on [contributing guidelines for tidymodels
    packages](https://www.tidymodels.org/contribute/) and [how to get
    help](https://www.tidymodels.org/help/).
