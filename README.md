
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adverbial (former partialised)

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/adverbial)](https://CRAN.R-project.org/package=adverbial)
[![R-CMD-check](https://github.com/UchidaMizuki/adverbial/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UchidaMizuki/adverbial/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/UchidaMizuki/adverbial/graph/badge.svg)](https://app.codecov.io/gh/UchidaMizuki/adverbial)
<!-- badges: end -->

adverbial provides `new_partialised()` and `new_composed()`, which
extend `partial()` and `compose()` functions of purrr to make it easier
to extract and replace arguments and functions, and has additional
adverbial functions such as `as_step()` for step-by-step data
processing.

## Installation

You can install the development version of adverbial from
[GitHub](https://github.com/) with:

``` r
# the released version from CRAN:
install.packages("adverbial")

# the development version from GitHub:
# install.packages("devtools")
devtools::install_github("UchidaMizuki/adverbial")
```

## Examples

``` r
library(adverbial)
```

### Enhanced partialised functions

`new_partialised()` is an enhanced version of `partial()` from purrr. It
allows you to extract and replace arguments of the function.

``` r
dist <- function(x, y) {
  sqrt(x ^ 2 + y ^ 2)
}

pdist <- new_partialised(
  dist,
  list(x = 3)
)
pdist
#> <partialised(1)>
#> function (x, y) 
#> {
#>     sqrt(x^2 + y^2)
#> }
#> (
#>   x = 3
#>   ...
#> )
pdist(y = 4)
#> [1] 5

# Get partialised arguments
pdist[]
#> $x
#> [1] 3
pdist$x
#> [1] 3
pdist$y
#> NULL

pdist$x <- 6
pdist(y = 8)
#> [1] 10

pdist$y <- 8
pdist()
#> [1] 10
```

### Enhanced composed functions

`new_composed()` is an enhanced version of `compose()` from purrr. It
allows you to extract and replace functions in the composition.

``` r
square <- function(x) x^2
cdist <- new_composed(
  list(
    square = square,
    sum = sum,
    sqrt = sqrt
  ),
  dir = "forward"
)
cdist
#> <composed(3)>
#> 1. square
#> function (x) 
#> x^2
#> 
#> 2. sum
#> function (..., na.rm = FALSE) 
#> .Primitive("sum")(..., na.rm = na.rm)
#> 
#> 3. sqrt
#> function (x) 
#> .Primitive("sqrt")(x)
cdist(1:10)
#> [1] 19.62142

# Get composed functions
cdist[]
#> $square
#> function (x) 
#> x^2
#> 
#> $sum
#> function (..., na.rm = FALSE) 
#> .Primitive("sum")(..., na.rm = na.rm)
#> 
#> $sqrt
#> function (x) 
#> .Primitive("sqrt")(x)
cdist$sum <- new_partialised(sum, list(na.rm = TRUE))

cdist(c(1:10, NA))
#> [1] 19.62142
```

### Step-by-step data processing

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

`step_by_step()` defines a step-by-step data processing pipeline by
passing a character vector with step names and descriptions.

`as_step()` converts an existing function into a step function that can
be used in a pipeline. Generated functions check if a step is correct
for objects created with `step-by-step()` and act as a normal function
for other objects. With `as_step(f)` (without passing a second argument)
you can add another function to step-by-step data processing.

`end_step()` is a function that can be used to end the step-by-step data
processing pipeline and return the result.

``` r
# Define a step-by-step data processing pipeline
dist_calculator <- step_by_step(c(
  square_step = "Square the input",
  sum_step = "Sum the squares",
  sqrt_step = "Take the square root"
))

# Define the steps
square_step <- as_step(function(x) x^2, "square_step")
sum_step <- as_step(sum, "sum_step")
sqrt_step <- as_step(sqrt, "sqrt_step")

square_step
#> <step: square_step>
#> function (x) 
#> x^2
sum_step
#> <step: sum_step>
#> function (..., na.rm = FALSE)  .Primitive("sum")
sqrt_step
#> <step: sqrt_step>
#> function (x)  .Primitive("sqrt")

dist <- dist_calculator(c(1:10, NA))
dist
#> # Steps:
#> # ☐ 1. square_step: Square the input
#> # ☐ 2. sum_step:    Sum the squares
#> # ☐ 3. sqrt_step:   Take the square root
#> # ℹ Please call `square_step()` to continue.
#> #
#>  [1]  1  2  3  4  5  6  7  8  9 10 NA

dist <- dist |> 
  square_step() |> 
  sum_step(na.rm = TRUE) |>
  sqrt_step()
dist
#> # Steps:
#> # ☒ 1. square_step: Square the input
#> # ☒ 2. sum_step:    Sum the squares
#> # ☒ 3. sqrt_step:   Take the square root
#> # ℹ All steps are done. Please call `end_step()`.
#> #
#> [1] 19.62142
end_step(dist)
#> [1] 19.62142
```
