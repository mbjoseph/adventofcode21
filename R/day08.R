#' Part 1
#'
#' @param file in file
#'
#' @examples
#' file <- system.file("extdata", "day8.txt", package = "adventofcode21")
#' easy_finder(file) # 303
#' decoded_sum(file) # 961734
#'
#' @export
easy_finder <- function(file) {
  char_counts <- readLines(file) %>%
    purrr::map_chr(~gsub(".*\\| ", "", .x)) %>%
    paste(collapse = " ") %>%
    strsplit(split = " ") %>%
    unlist() %>%
    nchar() %>%
    table()
  sum(char_counts[c("2", "3", "4", "7")])
}

#' Decode and sum
#'
#' @param file input file
#'
#' @importFrom dplyr summarize
#' @importFrom tidyr unnest
#'
#' @export
decoded_sum <- function(file) {
  long_rows <- readLines(file) %>%
    tibble::as_tibble() %>%
    tidyr::separate(value, into = c("input", "output"), sep = " \\| ") %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_longer(c("input", "output")) %>%
    dplyr::group_by(name, row) %>%
    dplyr::summarize(value = strsplit(value, split = " "), .groups = "drop") %>%
    tidyr::unnest(value) %>%
    # order doesn't tell us anything - we can sort without changing meaning
    dplyr::mutate(value = purrr::map_chr(value, sort_string))

  decoder <- long_rows %>%
    dplyr::filter(name == "input") %>%
    # grouping is necessary here, as the number of shared characters is
    # row-specific (each row has it's own signal mapping)
    dplyr::group_by(row) %>%
    dplyr::mutate(
      nc = nchar(value),
      n_shared_1 = purrr::map_int(value, ~n_shared_char(.x, value[nc == 2])),
      n_shared_4 = purrr::map_int(value, ~n_shared_char(.x, value[nc == 4])),
      n_shared_7 = purrr::map_int(value, ~n_shared_char(.x, value[nc == 3])),
      # deduce remaining digits based on number of characters (nc) and the
      # number of shared signals (letters) with known digit encodings
      digit = dplyr::case_when(
        nc == 6 & n_shared_1 == 2 & n_shared_4 == 3 & n_shared_7 == 3 ~ 0,
        nc == 2 ~ 1,
        nc == 5 & n_shared_1 == 1 & n_shared_4 == 2 & n_shared_7 == 2 ~ 2,
        nc == 5 & n_shared_1 == 2 & n_shared_4 == 3 & n_shared_7 == 3 ~ 3,
        nc == 4 ~ 4,
        nc == 5 & n_shared_1 == 1 & n_shared_4 == 3 & n_shared_7 == 2 ~ 5,
        nc == 6 & n_shared_7 == 2 & n_shared_1 == 1 ~ 6,
        nc == 3 ~ 7,
        nc == 7 ~ 8,
        nc == 6 & n_shared_1 == 2 & n_shared_4 == 4 & n_shared_7 == 3 ~ 9
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(row, value, digit)

  decoded_output <- long_rows %>%
    dplyr::filter(name == "output") %>%
    dplyr::left_join(decoder, by = c("row", "value"))

  # construct four digit numbers from the decoded digits and sum
  decoded_output %>%
    dplyr::group_by(row) %>%
    dplyr::summarize(digits = as.numeric(paste(digit, collapse = ""))) %>%
    dplyr::pull(digits) %>%
    sum()
}


n_shared_char <- function(x, y) {
  length(intersect(
    unlist(strsplit(x, "")),
    unlist(strsplit(y, ""))
  ))
}

sort_string <- function(x) {
  strsplit(x, "")[[1]] %>% sort %>% paste(collapse = "")
}
