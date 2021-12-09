file <- tempfile()

writeLines(
'2199943210
3987894921
9856789892
8767896789
9899965678
', file)

test_that("low_points works", {
  mins <- low_points(file)
  m <- read_mat(file)
  expect_equal(sum(c(m)[which(as.logical(mins))] + 1), 15)
})

test_that("basin_size works", {
  expect_equal(basin_size(file, 1, 2), 3)
  expect_equal(basin_size(file, 1, 9), 9)
  expect_equal(basin_size(file, 3, 3), 14)
  expect_equal(basin_size(file, 5, 7), 9)
})
