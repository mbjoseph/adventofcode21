file <- tempfile()
writeLines(
"NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C",
file
)

test_that("polymerize works", {
  expect_equal(polymerize(file, 10), "1588")
  expect_equal(polymerize(file, 40), "2188189693529")
})
