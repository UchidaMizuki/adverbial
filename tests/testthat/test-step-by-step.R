fns <- list(
  square = function(x) x^2,
  sum = sum,
  sqrt = sqrt
)

test_that("step-by-step works", {
  cdist <- new_composed(fns, dir = "forward")
  dist_calculator <- step_by_step(fns)

  x <- c(1:10, NA)
  dist <- dist_calculator(x)
  dist <- next_step(dist, "square")
  dist <- next_step(dist, "sum")
  dist <- next_step(dist, "sqrt")

  expect_equal(dist$data, cdist(x))
  expect_equal(dist$steps$state, rep("done", vctrs::vec_size(dist$steps)))

  fns_na_rm <- fns
  fns$sum <- new_partialised(sum, list(na.rm = TRUE))
  cdist_na_rm <- new_composed(fns, dir = "forward")

  dist <- dist_calculator(x)
  dist <- next_step(dist, "square")
  dist <- next_step(dist, "sum", na.rm = TRUE)
  dist <- next_step(dist, "sqrt")

  expect_equal(dist$data, cdist_na_rm(x))
  expect_equal(dist$steps$state, rep("done", vctrs::vec_size(dist$steps)))
})

test_that("next_step() works", {
  dist_calculator <- step_by_step(fns)

  x <- c(1:10, NA)
  dist <- dist_calculator(x)
  expect_error(next_step(dist, "square_wrong"))

  dist <- next_step(dist, "square")
  expect_error(next_step(dist, "square"))
})

test_that("insert_step() works", {
  dist_calculator <- step_by_step(
    fns,
    descriptions = c(
      "Square the input",
      "Sum the squares",
      "Take the square root"
    )
  )

  x <- c(1:10, NA)
  dist <- dist_calculator(x)

  dist_inserted <- insert_step(
    dist,
    list(sum2 = sum, sum3 = sum),
    descriptions = c("Sum 2", "Sum 3"),
    after = 2
  )

  expect_equal(dist_inserted$steps$step[3:4], c("sum2", "sum3"))
  expect_equal(dist_inserted$steps$description[3:4], c("Sum 2", "Sum 3"))

  expect_error({
    dist_inserted <- next_step(dist, "square")
    insert_step(dist_inserted, list(sum2 = sum, sum3 = sum), .after = 2)
  })
})


test_that("update_step() works", {
  dist_calculator <- step_by_step(
    fns,
    descriptions = c(
      "Square the input",
      "Sum the squares",
      "Take the square root"
    )
  )

  x <- c(1:10, NA)
  dist <- dist_calculator(x)

  dist_updated <- update_step(dist, 2, prod, description = "Product of squares")
  expect_equal(dist_updated$steps$fn[[2]], prod)
  expect_equal(dist_updated$steps$description[[2]], "Product of squares")

  expect_error({
    dist_updated <- next_step(dist, "square")
    update_step(dist_updated, 2, prod, description = "Product of squares")
  })
})

test_that("delete_step() works", {
  dist_calculator <- step_by_step(fns)

  x <- c(1:10, NA)
  dist <- dist_calculator(x)

  dist_deleted <- delete_step(dist, 2)
  expect_equal(dist_deleted$steps$step[[2]], "sqrt")

  expect_error({
    dist_deleted <- next_step(dist, "square")
    delete_step(dist_deleted, 2)
  })
})
