# Controlling the numeric details for conformal inference

Controlling the numeric details for conformal inference

## Usage

``` r
control_conformal_full(
  method = "iterative",
  trial_points = 100,
  var_multiplier = 10,
  max_iter = 100,
  tolerance = .Machine$double.eps^0.25,
  progress = FALSE,
  required_pkgs = character(0),
  seed = sample.int(10^5, 1)
)
```

## Arguments

- method:

  The method for computing the intervals. The options are `'search'`
  (using) [`stats::uniroot()`](https://rdrr.io/r/stats/uniroot.html),
  and `'grid'`.

- trial_points:

  When `method = "grid"`, how many points should be evaluated?

- var_multiplier:

  A multiplier for the variance model that determines the possible range
  of the bounds.

- max_iter:

  When `method = "iterative"`, the maximum number of iterations.

- tolerance:

  Tolerance value passed to
  [`all.equal()`](https://rdrr.io/r/base/all.equal.html) to determine
  convergence during the search computations.

- progress:

  Should a progress bar be used to track execution?

- required_pkgs:

  An optional character string for which packages are required.

- seed:

  A single integer used to control randomness when models are (re)fit.

## Value

A list object with the options given by the user.
