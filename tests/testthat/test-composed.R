test_that("composed dist", {
  square <- function(x) x^2
  cdist <- new_composed(
    list(
      square = square,
      sum = sum,
      sqrt = sqrt
    ),
    dir = "forward"
  )
  expect_equal(cdist(3:4), 5)
  expect_equal(cdist(c(3:4, NA)), NA_real_)

  cdist$sum <- new_partialised(sum, list(na.rm = TRUE))
  expect_equal(cdist(c(3:4, NA)), 5)
})
