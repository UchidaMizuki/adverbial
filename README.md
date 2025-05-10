
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adverbial (former partialised)

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/adverbial)](https://CRAN.R-project.org/package=adverbial)
<!-- badges: end -->

adverbial provides `new_partialised()` and `new_composed()`, which
extend the `partial()` and `compose()` functions of ‘purrr’ to make it
easier to extract and replace arguments and functions.

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

## Example

### Partialised functions

``` r
library(adverbial)

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

### Composed functions

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
