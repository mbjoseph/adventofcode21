#' fold dots
#'
#' @param file input file
#' @param times number of times to fold (NA to fold all)
#'
#' @importFrom dplyr distinct mutate_all
#' @importFrom readr parse_number
#' @importFrom stringr str_extract
#'
#' @export
fold <- function(file, times = NA) {
  raw_input <- readLines(file)

  coords <- grep(",", raw_input, value = TRUE)

  instructions <- grep("fold", raw_input, value = TRUE) %>%
    tibble() %>%
    separate(".", c("direction", "location"), sep = "=") %>%
    mutate(direction = str_extract(direction, "x|y"),
           location = parse_number(location))

  grid <- tibble(coords) %>%
    separate(coords, into = c("x", "y"), sep = ',') %>%
    mutate_all(as.numeric)

  # fold along y
  if (is.na(times)) times <- nrow(instructions)
  for (i in 1:times) {
    if (instructions$direction[i] == "x") {
      # fold along x axis
      grid <- grid %>%
        mutate(x_new = ifelse(x > instructions$location[i], x - 2 * (x - instructions$location[i]), x)) %>%
        distinct(x = x_new, y)
    } else {
      # fold along y axis
      grid <- grid %>%
        mutate(y_new = ifelse(y > instructions$location[i], y - 2 * (y - instructions$location[i]), y)) %>%
        distinct(x, y = y_new)
    }
  }

  grid
}
