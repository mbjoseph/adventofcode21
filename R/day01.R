#' Count number of increasing values in a sequence
#'
#' @param x a numeric sequence
#'
#' @examples
#' f <- system.file("extdata", "day1.txt", package = "adventofcode21")
#' x <- read.delim(f, header = FALSE)[[1]]
#' count_increased(x) # 1451
#'
#' @export
count_increased <- function(x) {
  sum(diff(x) > 0, na.rm = TRUE)
}


#' Compute a sliding sum of a sequence
#'
#' @param x a sequence
#' @param width the width of the sliding sum
#'
#' @examples
#' f <- system.file("extdata", "day1.txt", package = "adventofcode21")
#' x <- read.delim(f, header = FALSE)[[1]]
#' lag_sums <- sliding_sum(x, width = 3)
#' count_increased(lag_sums) # 1395
#'
#'@export
sliding_sum <- function(x, width) {
  lag_matrix <- cbind(
    x,
    dplyr::lag(x, n = 1L),
    dplyr::lag(x, n = 2L)
  )
  rowSums(lag_matrix)
}
