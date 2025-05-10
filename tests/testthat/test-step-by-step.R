test_that("step-by-step works", {
  fns <- list(
    square = function(x) x^2,
    sum = sum,
    sqrt = sqrt
  )
  cdist <- new_composed(fns, dir = "forward")

  dist_calculator <- step_by_step(c(
    square_step = "Square the input",
    sum_step = "Sum the squares",
    sqrt_step = "Take the square root"
  ))
  square_step <- as_step(function(x) x^2, "square_step")
  sum_step <- as_step(sum, "sum_step")
  sqrt_step <- as_step(sqrt, "sqrt_step")

  x <- c(1:10, NA)
  dist <- dist_calculator(x)
  dist <- square_step(dist)
  dist <- sum_step(dist)
  dist <- sqrt_step(dist)
  dist <- end_step(dist)

  expect_equal(dist, cdist(x))

  fns_na_rm <- fns
  fns$sum <- new_partialised(sum, list(na.rm = TRUE))
  cdist_na_rm <- new_composed(fns, dir = "forward")

  dist <- dist_calculator(x)
  dist <- square_step(dist)
  dist <- sum_step(dist, na.rm = TRUE)
  dist <- sqrt_step(dist)
  dist <- end_step(dist)

  expect_equal(dist, cdist_na_rm(x))

  dist <- dist_calculator(x)
  expect_error(sum_step(dist))
  expect_error(sqrt_step(dist))
})
