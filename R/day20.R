#' image 'enhancement'
#'
#' @param file input file
#' @param times number of times to enhance
#'
#' @import dplyr
#' @importFrom dplyr group_by row_number ungroup
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "day20.txt", package = "adventofcode21")
#' enhance_image(file, times = 2)
#' }
#'
#' @export
enhance_image <- function(file, times) {
  algo <- readLines(file, n = 1)

  algo_lookup <- unlist(strsplit(algo, split = ""))
  hashes <- which(algo_lookup == "#")

  input_image <- readLines(file)[-c(1:2)] %>%
    as_tibble() %>%
    mutate(
      row = row_number(),
      value = strsplit(value, "")
    ) %>%
    unnest(value) %>%
    group_by(row) %>%
    mutate(col = 1:n()) %>%
    ungroup() %>%
    filter(value == "#")


  # the infinite void starts off as a .
  void <- "."

  for (i in 1:times) {

    min_row <- min(input_image$row)
    max_row <- max(input_image$row)
    min_col <- min(input_image$col)
    max_col <- max(input_image$col)


    out <- tidyr::expand_grid(
      row = (min_row - 2):(max_row + 2),
      col = (min_col - 2):(max_col + 2),
    ) %>%
      group_by(row, col) %>%
      summarize(
        nb_row = row + rep(c(-1, 0, 1), each = 3),
        nb_col = col + rep(c(-1, 0, 1), 3)
      ) %>%
      ungroup() %>%
      left_join(input_image, by = c("nb_row" = "row", "nb_col" = "col")) %>%
      mutate(
        row_in = between(nb_row, min_row, max_row),
        col_in = between(nb_col, min_col, max_col),
        in_grid = row_in & col_in,
        # if we are extracting values outside of our input grid, use void
        # otherwise, use "."
        value = case_when(
          is.na(value) & in_grid ~ ".",
          is.na(value) & !in_grid ~ void,
          TRUE ~ value
        ),
        value = ifelse(value == ".", 0, 1)) %>%
      group_by(row, col) %>%
      summarize(bin_string = paste0(value, collapse = ""), .groups = "drop") %>%
      mutate(
        algo_index = strtoi(bin_string, base = 2L) + 1
      ) %>%
      filter(algo_index %in% hashes) %>%
      select(row, col) %>%
      mutate(value = "#")

    if (void == ".") {
      void <- strtoi(c("000000000"), base = 2L) %>%
        as.numeric() %>%
        `+`(1) %>%
        substr(algo, ., .)
    } else {
      # the void is on
      void <- strtoi(c("111111111"), base = 2L) %>%
        as.numeric() %>%
        `+`(1) %>%
        substr(algo, ., .)
    }
    input_image <- out
  }

  nrow(input_image)
}
