% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/composed.R
\name{new_composed}
\alias{new_composed}
\title{Create composed functions}
\usage{
new_composed(fns, dir = NULL, ..., class = character())
}
\arguments{
\item{fns}{A list of functions to compose.}

\item{dir}{Direction of composition, either \code{"forward"} or \code{"backward"}.
By default, the functions are composed in the forward direction.
Passed to \code{\link[purrr:compose]{purrr::compose()}}.}

\item{...}{Additional arguments for attributes.}

\item{class}{Name of subclass.}
}
\value{
A composed function that inherits from \code{adverbial_function_compose}.
}
\description{
Create composed functions
}
\examples{
square <- function(x) x ^ 2
cdist <- new_composed(list(square = square, sum = sum, sqrt = sqrt))
cdist(1:10)

cdist$sum <- new_partialised(sum, list(na.rm = TRUE))
cdist(c(1:10, NA))

}
\seealso{
\code{\link[purrr:compose]{purrr::compose()}}
}
