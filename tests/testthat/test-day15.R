file <- tempfile()

writeLines(
"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581", file
)

test_that("least cost path works", {
  expect_equal(dijkstra(read_mat(file)), 40)
})
