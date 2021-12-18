test_that("hex to bits works", {
  expect_equal(hex2bits("D2FE28"), "110100101111111000101000")
  expect_equal(
    hex2bits("38006F45291200"),
    "00111000000000000110111101000101001010010001001000000000"
  )
  expect_equal(
    hex2bits("EE00D40C823060"),
    "11101110000000001101010000001100100000100011000001100000"
  )
})

version_sum <- function(hex) {
  out <- hex2bits(hex) %>%
     read_packets()
  sum(out$versions)
}

test_that("version sum works", {
  expect_equal(version_sum("8A004A801A8002F478"), 16)
  expect_equal(version_sum("620080001611562C8802118E34"), 12)
  expect_equal(version_sum("C0015000016115A2E0802F182340"), 23)
  expect_equal(version_sum("A0016C880162017C3686B18A3D4780"), 31)
})
