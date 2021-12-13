file <- tempfile()

writeLines(
"start-A
start-b
A-c
A-b
b-d
A-end
b-end", file
)

test_that("find_all_paths works", {
  expect_equal(find_all_paths(file), 10)
})

test_that("find_twofer_paths works", {
  expect_equal(find_twofer_paths(file), 36)
})

