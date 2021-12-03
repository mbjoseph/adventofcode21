#' Part 1
#'
#' @param f input file
#'
#' @examples
#' f <- system.file("extdata", "day3.txt", package = "adventofcode21")
#' gamma_epsilon_prod(f) # 2003336
#'
#' @export
gamma_epsilon_prod <- function(f) {
  column_means <- in_to_matrix(f) %>%
    colMeans()

  assertthat::assert_that(!any(column_means == 0.5))
  gamma <- as.numeric(column_means > 0.5)
  epsilon <- as.numeric(!gamma)

  bitsToInt(gamma) * bitsToInt(epsilon)
}

#' Part 2
#'
#' @param f input file
#'
#' @examples
#' f <- system.file("extdata", "day3.txt", package = "adventofcode21")
#' o2_co2_prod(f) # 1877139
#' @export
o2_co2_prod <- function(f) {
  in_mat <- in_to_matrix(f)

  colnames(in_mat) <- paste0("d", 1:ncol(in_mat))

  o2 <- co2 <- in_df <- tibble::as_tibble(in_mat)

  o2_tiebreaker <- 1
  co2_tiebreaker <- 0

  for (i in 1:ncol(in_df)) {
    col <- colnames(o2)[i]

    if (nrow(o2) != 1) {

      mfvs <- statip::mfv(o2[[col]])
      tie <- length(mfvs) > 1

      if (tie) {
        o2 <- o2[o2[[col]] == o2_tiebreaker, ]
      } else {
        o2 <- o2[o2[[col]] == mfvs, ]
      }
    }

    if (nrow(co2) != 1) {

      mfvs <- statip::mfv(co2[[col]])
      tie <- length(mfvs) > 1

      if (tie) {
        co2 <- co2[co2[[col]] == co2_tiebreaker, ]
      } else {
        co2 <- co2[co2[[col]] != mfvs, ]
      }
    }
  }

  bitsToInt(o2[1, ]) * bitsToInt(co2[1, ])
}


#' convert input file to matrix
#'
#' @param f input file
#' @export
in_to_matrix <- function(f) {
  rows <- readLines(f) %>%
    lapply(strsplit, split = "") %>%
    lapply(unlist) %>%
    lapply(as.numeric) %>%
    lapply(matrix, nrow = 1)

  do.call(rbind, rows)
}


#https://stackoverflow.com/questions/25411380/convert-binary-vector-to-decimal
bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}
