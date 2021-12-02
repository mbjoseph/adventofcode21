test_that("regex_sum works", {
  x <- c("a 1", "a 2", "b 1", "b 5")
  expect_equal(regex_sum("a", x), 3)
  expect_equal(regex_sum("b", x), 6)
})

test_that("get_aimed_result works", {
  instructions <- c(
    "forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2"
  )
  expect_equal(get_aimed_result(instructions), 900)
})
