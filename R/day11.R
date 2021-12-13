#' part 1
#'
#' @param file input file
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "day11.txt", package = "adventofcode21")
#' count_flashes(file)}
#'
#'
#' @export
count_flashes <- function(file) {
  m <- read_mat(file)
  flashes <- 0

  for (i in 1:100) {
    m <- m + 1

    initial_flashers <- m > 9

    if (!any(initial_flashers)) {
      next
    }

    flashed <- which(initial_flashers, arr.ind = TRUE) %>%
      data.frame() %>%
      dplyr::mutate(propagated = FALSE)

    repeat {
      adj_df <- flashed %>%
        dplyr::filter(!propagated) %>%
        dplyr::rowwise() %>%
        dplyr::summarize(get_neighboring_indices(row, col)) %>%
        dplyr::anti_join(flashed, by = c("row", "col")) %>%
        dplyr::filter(
          dplyr::between(row, 1, nrow(m)),
          dplyr::between(col, 1, ncol(m))
        ) %>%
        dplyr::count(row, col)

      m[cbind(adj_df$row, adj_df$col)] <- m[cbind(adj_df$row, adj_df$col)] +
        adj_df$n

      flashed$propagated <- TRUE # we propagated the flashes ^

      flashed <- data.frame(which(m > 9, arr.ind = TRUE)) %>%
        dplyr::left_join(flashed, by = c("row", "col")) %>%
        dplyr::mutate(propagated = ifelse(is.na(propagated), FALSE, propagated))

      if (all(flashed$propagated)) {
        break
      }
    }

    flashes <- flashes + sum(m > 9)

    m[m > 9] <- 0
  }

  flashes
}




#' part 2: find when all things flash
#'
#' @param file input file
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "day11.txt", package = "adventofcode21")
#' find_all_flash(file)
#' }
#'
#' @export
find_all_flash <- function(file) {
  m <- read_mat(file)
  flashes <- 0

  i <- 0
  while(!all(m == 0)) {
    i <- i + 1
    m <- m + 1

    initial_flashers <- m > 9

    if (!any(initial_flashers)) {
      next
    }

    flashed <- which(initial_flashers, arr.ind = TRUE) %>%
      data.frame() %>%
      dplyr::mutate(propagated = FALSE)

    repeat {
      adj_df <- flashed %>%
        dplyr::filter(!propagated) %>%
        dplyr::rowwise() %>%
        dplyr::summarize(get_neighboring_indices(row, col)) %>%
        dplyr::anti_join(flashed, by = c("row", "col")) %>%
        dplyr::filter(
          dplyr::between(row, 1, nrow(m)),
          dplyr::between(col, 1, ncol(m))
        ) %>%
        dplyr::count(row, col)

      m[cbind(adj_df$row, adj_df$col)] <- m[cbind(adj_df$row, adj_df$col)] +
        adj_df$n

      flashed$propagated <- TRUE # we propagated the flashes ^

      flashed <- data.frame(which(m > 9, arr.ind = TRUE)) %>%
        dplyr::left_join(flashed, by = c("row", "col")) %>%
        dplyr::mutate(propagated = ifelse(is.na(propagated), FALSE, propagated))

      if (all(flashed$propagated)) {
        break
      }
    }

    flashes <- flashes + sum(m > 9)

    m[m > 9] <- 0

    if (all(m == 0)) return(i)
  }
}


get_neighboring_indices <- function(row, col) {
  data.frame(
    row = row + c(-1, -1, -1, 0, 1, 1, 1, 0),
    col = col + c(-1, 0, 1, 1, 1, 0, -1, -1)
  )
}

