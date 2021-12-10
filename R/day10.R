#' Compute the corruption score (part 1)
#'
#' @param file input file
#'
#' @examples
#' corruption_score(system.file("extdata", "day10.txt", package = "adventofcode21"))
#' @export
corruption_score <- function(file) {

  x <- remove_valid_pairs(file)

  # extract mismatched pairs and compute score
  point_lookup <- c("\\)" = "3", "\\]" = "57", "\\}" = "1197", ">" = "25137")

  x %>%
    stringr::str_extract(
      pattern = make_invalid_pairs()
    ) %>%
    stringr::str_sub(start = 2, end = 2) %>%
    stringr::str_replace_all(point_lookup) %>%
    as.numeric() %>%
    sum(na.rm = TRUE)
}


#' Compute the completion score (part 2)
#'
#' @param file input file
#'
#' @examples
#' completion_score(system.file("extdata", "day10.txt", package = "adventofcode21"))
#'
#' @export
completion_score <- function(file) {

  x <- remove_valid_pairs(file)

  reversal_lookup <- c("\\[" = "]", "\\(" = ")", "\\{" = "}", "<" = ">")
  score_lookup = c("\\)" = "1", "\\]" = "2", "\\}" = "3", ">" = "4")

  # identify corrupt and remove
  incomplete <- x %>%
    stringr::str_extract(pattern = make_invalid_pairs()) %>%
    is.na()

  incomplete_entries <- x[incomplete]

  incomplete_entries %>%
    stringr::str_replace_all(reversal_lookup) %>%
    stringi::stri_reverse() %>%
    stringr::str_replace_all(score_lookup) %>%
    stringr::str_split("") %>%
    purrr::map(as.numeric) %>%
    purrr::map_dbl(score_string) %>%
    median()
}

make_invalid_pairs <- function() {
  opening <- c("\\[", "\\(", "\\{", "<")
  closing <- c("\\]", "\\)", "\\}", ">")
  valid_pairs <- purrr::map2_chr(opening, closing, ~paste0(.x, .y, collapse = ""))

  pairs <- tidyr::expand_grid(
    left = opening, right = closing
  ) %>%
    tidyr::unite(pair, left, right, sep = '') %>%
    dplyr::mutate(is_valid = pair %in% valid_pairs)

  invalid_pairs <- pairs$pair[!pairs$is_valid]

  invalid_pairs %>%
    paste0(collapse = "|")
}

remove_valid_pairs <- function(file) {

  x <- readLines(file)

  for (i in 1:50) {
    x_new <- stringr::str_replace_all(
      x,
      c("\\[\\]" = "", "<>" = "", "\\{\\}" = "", "\\(\\)" = "")
    )
    if (identical(x_new, x)) {
      break
    } else {
      x <- x_new
    }
  }

  x
}


score_string <- function(vals) {
  score <- 0
  for (i in seq_along(vals)) {
    score <- 5 * score + vals[i]
  }
  score
}
