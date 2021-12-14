#' polymer fun
#'
#' @param file input file
#' @param steps number of steps to run
#'
#' @examples
#' file <- system.file("extdata", "day14.txt", package = "adventofcode21")
#' polymerize(file, 10)
#' polymerize(file, 40)
#'
#' @importFrom dplyr bind_rows inner_join select
#' @importFrom tidyr pivot_longer
#'
#' @export
polymerize <- function(file, steps) {

  instructions <- readr::read_fwf(file, skip = 1)
  names(instructions) <- c("pair", "arrow", "insert")

  instructions <- instructions %>%
    mutate(
      insertion = purrr::map2(pair, insert, insert_chr)
    ) %>%
    select(pair, insertion) %>%
    unnest(insertion)

  sequence <- readLines(file, n = 1)
  tuples <- rep(NA, nchar(sequence) - 1)
  for (i in 1:(nchar(sequence) - 1)) {
    tuples[i] <- substring(sequence, i, i+1)
  }

  counter <- tibble(pair = tuples) %>%
    count(pair)

  for (t in 1:steps) {
    counter <- inner_join(instructions, counter, by = "pair") %>%
      select(child_1, child_2, n) %>%
      pivot_longer(starts_with("child"), names_to = "child", values_to = "pair") %>%
      count(pair, wt = n)
  }

  # tally occurrences, adjusting for the fact that all characters are double
  # counted (because tuples), except start and end characters in the sequence
  start_adjustment <- tibble(
    value = c(
      substr(sequence, 1, 1),
      substr(sequence, nchar(sequence), nchar(sequence))
    ),
    n = 0.5
  )

  counter %>%
    mutate(first_char = substr(pair, 1, 1), second_char = substr(pair, 2, 2)) %>%
    select(n, ends_with("char")) %>%
    pivot_longer(ends_with("char")) %>%
    count(value, wt = n) %>%
    mutate(n = n / 2) %>%
    bind_rows(start_adjustment) %>%
    count(value, wt = n) %>%
    summarize(max(n) - min(n)) %>%
    unlist() %>%
    format(scientific = F) %>%
    unname()
}

insert_chr <- function(x, y) {
  tibble(
    child_1 = c(substr(x, 1, 1), y) %>% paste(collapse = ""),
    child_2 = c(y, substr(x, 2, 2)) %>% paste(collapse = "")
  )
}
