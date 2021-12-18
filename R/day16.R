#' best AoC ever
#'
#' @param bits input bits
#' @param versions initializes to NULL but changes when called recursively
#'
#' @examples
#' out <- system.file("extdata", "day16.txt", package = "adventofcode21") %>%
#'   readLines() %>%
#'   hex2bits() %>%
#'   read_packets()
#' sum(out$versions)
#' out$value # 539051801941
#'
#' @importFrom Rmpfr mpfr
#'
#' @export
read_packets <- function(bits, versions = NULL) {
  if (nchar(bits) < 6) {
    return(list(bits = bits, versions = versions))
  }

  version <- substr(bits, 1, 3) %>%
    strtoi(base = 2)
  versions <- c(versions, version)
  bits <- substr(bits, 4, nchar(bits))

  type_id <- substr(bits, 1, 3) %>%
    strtoi(base = 2)
  bits <- substr(bits, 4, nchar(bits))

  if (type_id == 4) {
    literal_val <- read_literal_value(bits)

    return(
      list(
        bits = literal_val$bits,
        versions = versions,
        value = literal_val$value
      )
    )

  } else {
    # we are reading an operator
    length_type_id <- substr(bits, 1, 1)
    bits <- substr(bits, 2, nchar(bits))

    subpackets <- list()

    if (length_type_id == "0") {
      # next 15 bits are a number that represents the total length in bits of
      # the sub-packets contained by this packet.
      total_length_of_subpackets <- substr(bits, 1, 15) %>%
        strtoi(base  = 2)
      bits <- substr(bits, 16, nchar(bits))

      orig_nchar <- nchar(bits)

      while((orig_nchar - nchar(bits)) < total_length_of_subpackets) {
        tmp_pack <- read_packets(bits, versions)

        bits <- tmp_pack$bits
        versions <- tmp_pack$versions
        subpackets[[length(subpackets) + 1]] <- tmp_pack
      }
    } else {
      # next 11 bits are a number that represents the number of sub-packets
      # immediately contained by this packet.
      number_of_subpackets <- substr(bits, 1, 11) %>%
        strtoi(base = 2)
      bits <- substr(bits, 12, nchar(bits))

      for (i in 1:number_of_subpackets) {
        tmp_pack <- read_packets(bits, versions)

        bits <- tmp_pack$bits
        versions <- tmp_pack$versions
        subpackets[[length(subpackets) + 1]] <- tmp_pack
      }
    }
  }

  subpack_vals <- lapply(
    subpackets,
    FUN = function(x) gmp::asNumeric(x$value)
  ) %>%
    unlist()

  value <- case_when(
    type_id == 0 ~ sum(subpack_vals),
    type_id == 1 ~ prod(subpack_vals),
    type_id == 2 ~ min(subpack_vals),
    type_id == 3 ~ max(subpack_vals),
    type_id == 5 ~ as.numeric(subpack_vals[1] > subpack_vals[2]),
    type_id == 6 ~ as.numeric(subpack_vals[1] < subpack_vals[2]),
    type_id == 7 ~ as.numeric(subpack_vals[1] == subpack_vals[2])
  )

  list(
    bits = bits,
    versions = versions,
    value = value
  )
}


#' Convert hexadecimal to bits
#'
#' @param hex input hex data
#'
#' @export
hex2bits <- function(hex) {
  dict <-
"0 = 0000
1 = 0001
2 = 0010
3 = 0011
4 = 0100
5 = 0101
6 = 0110
7 = 0111
8 = 1000
9 = 1001
A = 1010
B = 1011
C = 1100
D = 1101
E = 1110
F = 1111" %>%
    strsplit("\n") %>%
    unlist() %>%
    tibble(key = .) %>%
    separate(key, into = c("hex", "bin"), sep = " = ")

  split_hex <- hex %>%
    strsplit("") %>%
    unlist()

  dict$bin[match(split_hex, dict$hex)] %>%
    paste0(collapse = "")
}


read_literal_value <- function(bits) {

  stop <- FALSE
  idx <- 1
  out <- ""

  while(!stop) {
    next_five_bits <- substr(bits, idx, idx + 4)
    stop <- substr(next_five_bits, 1, 1) == "0"
    out <- paste0(out, substr(next_five_bits, 2, 5))
    idx <- idx + 5
  }

  list(
    bits = substr(bits, idx, nchar(bits)),
    value = mpfr(out, 1024, base = 2)
  )
}
