test_that("partialised dist", {
  dist <- function(x, y) {
    sqrt(x^2 + y^2)
  }

  pdist <- new_partialised(dist, list(x = 3))
  expect_equal(pdist(y = 4), 5)

  pdist$x <- 6
  expect_equal(pdist(y = 8), 10)

  pdist$y <- 8
  expect_equal(pdist(), 10)
})
