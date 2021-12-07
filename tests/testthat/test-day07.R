init <- c(16,1,2,0,4,2,7,1,2,14)

test_that("basic works", {
  expect_equal(least_cost(init), 37)
})

test_that("fancy works", {
  expect_equal(fancy_least_cost(init), 168)
})

