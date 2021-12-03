f <- tempfile()
x <- "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010" %>%
  writeLines(f)

test_that("pt 1 works", {
  expect_equal(gamma_epsilon_prod(f), 198)
})


test_that("pt 2 works", {
  expect_equal(o2_co2_prod(f), 230)
})
