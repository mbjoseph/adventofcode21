#' find low points
#'
#' @param file input
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "day9.txt", package = "adventofcode21")
#' m <- read_mat(file)
#' mins <- low_points(file)
#'
#' # part 1
#' sum(c(m)[which(as.logical(mins))] + 1) #560
#'
#' # part 2
#' prod_biggest_basins(file) # 959136
#' }
#'
#'
#'
#' @export
low_points <- function(file) {

  m <- read_mat(file)

  low_x <- matrix(0, nrow = nrow(m), ncol = ncol(m))
  low_y <- matrix(0, nrow = nrow(m), ncol = ncol(m))
  for (i in 1:nrow(m)) {
    low_x[i, ] <- diff(sign(diff(c(Inf, m[i, ], Inf)))) > 0
  }
  for (i in 1:ncol(m)) {
    low_y[, i] <- diff(sign(diff(c(Inf, m[, i], Inf)))) > 0
  }

  is_local_min <- low_x * low_y
  is_local_min
}

#' find the size of a basin
#'
#' @param file input file
#' @param xpos x position of minimum
#' @param ypos y position of minimum
#'
#' @export
basin_size <- function(file, xpos, ypos) {
  m <- read_mat(file)
  rows <- matrix(1:nrow(m), nrow = nrow(m), ncol = ncol(m))
  cols <- matrix(rep(1:ncol(m), each = nrow(m)), nrow = nrow(m), ncol = ncol(m))

  visited <- matrix(0, nrow = nrow(m), ncol = ncol(m))
  in_watershed <- matrix(NA, nrow = nrow(m), ncol = ncol(m))
  in_watershed[xpos, ypos] <- 1

  # grow watershed
  to_visit <- in_watershed == 1 & visited == 0

  while(any(to_visit, na.rm = TRUE)) {
    which_to_visit <- which(to_visit == 1)[1]
    row_to_visit <- rows[which_to_visit]
    col_to_visit <- cols[which_to_visit]

    # flag neighbors to visit
    neighbors_to_visit <- data.frame(
      row = c(row_to_visit - 1, row_to_visit + 1, row_to_visit, row_to_visit),
      col = c(col_to_visit, col_to_visit, col_to_visit - 1, col_to_visit + 1)
    ) %>%
      dplyr::filter(
        dplyr::between(row, 1, nrow(m)),
        dplyr::between(col, 1, ncol(m))) %>%
      dplyr::mutate(
        val = m[cbind(row, col)],
        in_ws = in_watershed[cbind(row, col)],
        visited = visited[cbind(row, col)]
      ) %>%
      dplyr::filter(val != 9, visited == 0)

    if (nrow(neighbors_to_visit) > 0) {
      to_visit[cbind(neighbors_to_visit$row, neighbors_to_visit$col)] <- TRUE
    }

    # if we don't know whether the point is in the watershed, find out
    if (is.na(in_watershed[row_to_visit, col_to_visit])) {
      if (m[row_to_visit, col_to_visit] == 9) {
        in_watershed[row_to_visit, col_to_visit] <- 0
      } else {
        # are any neighbors in?
        if (!all(is.na(neighbors_to_visit$in_ws))) {
          if (any(neighbors_to_visit$in_ws == 1)) {
            in_watershed[row_to_visit, col_to_visit] <- 1
          }
        }
      }
    }

    # mark that this position has been visited
    visited[row_to_visit, col_to_visit] <- 1

    # update the set of things to be visited
    to_visit[row_to_visit, col_to_visit] <- FALSE
  }

  sum(to_visit == FALSE, na.rm = TRUE)
}

#' Find product of sizes for three largest basins
#'
#' @param file input file
#'
#' @export
prod_biggest_basins <- function(file) {
  m <- read_mat(file)
  minima <- low_points(file)
  rows <- matrix(1:nrow(m), nrow = nrow(m), ncol = ncol(m))
  cols <- matrix(rep(1:ncol(m), each = nrow(m)), nrow = nrow(m), ncol = ncol(m))

  minima_to_visit <- tibble::tibble(
    row = rows[minima == 1],
    col = cols[minima == 1],
    ws_size = 0
  )

  for (i in 1:nrow(minima_to_visit)) {
    minima_to_visit$ws_size[i] <- basin_size(file, minima_to_visit$row[i], minima_to_visit$col[i])
  }

  dplyr::slice_max(minima_to_visit, ws_size, n = 3) %>%
    dplyr::pull(ws_size) %>%
    prod
}

#' Read text matrix
#'
#' @param file input file
#'
#' @export
read_mat <- function(file) {
  readLines(file) %>%
    purrr::map(~as.numeric(unlist(strsplit(.x, split = "")))) %>%
    do.call(rbind, args = .)
}
