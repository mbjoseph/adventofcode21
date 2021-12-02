x <- 0:3
y <- rep(0:1, 3)
z <- 2:(-2)

test_that("count_increased works", {
  expect_equal(count_increased(x), 3)

  expect_equal(count_increased(y), 3)

  expect_equal(count_increased(z), 0)
})

test_that("sliding_sum works", {
  expect_equal(
    sliding_sum(x, width = 3),
    c(NA, NA, 3, 6)
  )

  expect_equal(
    sliding_sum(y, width = 3),
    c(NA, NA, 1, 2, 1, 2)
  )

  expect_equal(
    sliding_sum(z, width = 3),
    c(NA, NA, 3, 0, -3)
  )
})
