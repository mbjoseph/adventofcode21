file <- tempfile()

writeLines(
"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526",
file
)

test_that("count_flashes works", {
  expect_equal(count_flashes(file), 1656)
})
