#' Extract numbers from character vector matching a regex, then sum
#'
#' @param pattern regular expression
#' @param x character vector
#'
#' @examples
#' f <- system.file("extdata", "day2.txt", package = "adventofcode21")
#' instructions <- read.delim(f, header = FALSE)[[1]]
#' x_position <- regex_sum("forward", instructions)
#' y_positive <- regex_sum("down", instructions)
#' y_negative <- regex_sum("up", instructions)
#' y_position <- y_positive - y_negative
#' x_position * y_position # 1938402
#' @export
regex_sum <- function(pattern, x) {
  grep(pattern, x, value = TRUE) %>%
    readr::parse_number() %>%
    sum()
}


#' Get aimed result
#'
#' @param instructions character vector
#'
#' @examples
#' f <- system.file("extdata", "day2.txt", package = "adventofcode21")
#' instructions <- read.delim(f, header = FALSE)[[1]]
#' get_aimed_result(instructions) # 1947878632
#'
#' @export
get_aimed_result <- function(instructions) {
  y <- 0
  aim <- 0

  for (i in instructions) {
    num <- readr::parse_number(i)
    if (grepl("forward", i)) {
      y <- y + aim * num
    } else {
      if (grepl("up", i)) {
        aim <- aim - num
      } else {
        aim <- aim + num
      }
    }
  }

  regex_sum("forward", instructions) * y
}
