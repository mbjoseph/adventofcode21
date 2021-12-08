#' Part 1
#'
#' @param file in file
#'
#' @examples
#' f(system.file("extdata", "day8.txt", package = "adventofcode21")) 303
#' f2(system.file("extdata", "day8.txt", package = "adventofcode21")) 961734
#'
#' @export
f <- function(file) {
  d <- readLines(file)
  char_counts <- purrr::map_chr(d, ~gsub(".*\\| ", "", .x)) %>%
    paste(collapse = " ") %>%
    strsplit(split = " ") %>%
    unlist() %>%
    nchar() %>%
    table()
  sum(char_counts[c("2", "3", "4", "7")])
}

f2 <- function(file) {
  d <- readLines(file)

  vals <- rep(NA, length(d))

  for (i in seq_along(d)) {
    long_row <- d[i] %>%
      as_tibble() %>%
      separate(value, into = c("input", "output"), sep = " \\| ") %>%
      pivot_longer(everything()) %>%
      group_by(name) %>%
      summarize(value = strsplit(value, split = " ")) %>%
      unnest(value) %>%
      mutate(value = purrr::map_chr(value, sort_string))

    decoder <- long_row %>%
      filter(name == "input") %>%
      mutate(
        nc = nchar(value),
        n_sh_1 = purrr::map_int(value, ~n_shared_char(.x, value[nc == 2])),
        n_sh_4 = purrr::map_int(value, ~n_shared_char(.x, value[nc == 4])),
        n_sh_7 = purrr::map_int(value, ~n_shared_char(.x, value[nc == 3])),
        digit = case_when(
          nc == 6 & n_sh_1 == 2 & n_sh_4 == 3 & n_sh_7 == 3 ~ 0,
          nc == 2 ~ 1,
          nc == 5 & n_sh_1 == 1 & n_sh_4 == 2 & n_sh_7 == 2 ~ 2,
          nc == 5 & n_sh_1 == 2 & n_sh_4 == 3 & n_sh_7 == 3 ~ 3,
          nc == 4 ~ 4,
          nc == 5 & n_sh_1 == 1 & n_sh_4 == 3 & n_sh_7 == 2 ~ 5,
          nc == 6 & n_sh_7 == 2 & n_sh_1 == 1 ~ 6,
          nc == 3 ~ 7,
          nc == 7 ~ 8,
          nc == 6 & n_sh_1 == 2 & n_sh_4 == 4 & n_sh_7 == 3 ~ 9
        )
      ) %>%
      select(value, digit)

    decoded_output <- long_row %>%
      filter(name == "output") %>%
      left_join(decoder, by = "value")

    vals[i] <- paste(decoded_output$digit, collapse = "") %>%
      as.numeric()
  }

  sum(vals)
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
