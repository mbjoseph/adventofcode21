f <- system.file("extdata", "day4_ex.txt", package = "adventofcode21")


test_that("part 1 works", {
  expect_equal(
    pick_board(f, goal = "win"), 4512)
})

test_that("part 2 works", {
  expect_equal(
    pick_board(f, goal = "lose"), 1924
  )
})
