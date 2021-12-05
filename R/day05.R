#' part 1
#'
#' @param f input file
#' @param allow_diagonals whether to permit diagonal lines
#'
#' @examples
#' f <- system.file("extdata", "day5.txt", package = "adventofcode21")
#' pick_overlap(f, allow_diagonals = FALSE)
#' pick_overlap(f, allow_diagonals = TRUE)
#'
#' @export
pick_overlap <- function(f, allow_diagonals = TRUE) {

  all_lines <- readLines(f) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(line = dplyr::row_number()) %>%
    tidyr::separate(value, into = c("start", "end"), sep = " -> ") %>%
    tidyr::pivot_longer(cols = c(start, end)) %>%
    tidyr::separate(value, into = c("x", "y"), sep = ",") %>%
    dplyr::group_by(line)

  if (!allow_diagonals) {
    all_lines <- all_lines %>%
      dplyr::filter(
        dplyr::n_distinct(x) == 1 | dplyr::n_distinct(y) == 1
      )
  }

  all_lines %>%
    dplyr::summarize(
      x = x[name == "start"]:x[name == "end"],
      y = y[name == "start"]:y[name == "end"],
      .groups = "drop"
    ) %>%
    dplyr::count(x, y) %>%
    dplyr::summarize(n = sum(n >= 2)) %>%
    dplyr::pull(n)
}
