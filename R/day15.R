#' Compute least cost path using Dijkstra's algorithm
#'
#' @param cost cost matrix
#'
#' @examples
#' file <- system.file("extdata", "day15.txt", package = "adventofcode21")
#' cost <- read_mat(file)
#' dijkstra(cost)
#'
#' @export

dijkstra <- function(cost) {
  nr <- nrow(cost)
  nc <- ncol(cost)

  visited <- matrix(FALSE, nrow = nr, ncol = nc)
  tentative_cost <- matrix(NA, nrow = nr, ncol = nc)
  rows <- matrix(1:nr, nrow = nr, ncol = nc)
  cols <- matrix(1:nc, nrow = nr, ncol = nc, byrow = TRUE)

  # initialize
  tentative_cost[1, 1] <- 0

  while(is.na(tentative_cost[nr, nc])) {
    tmp <- tentative_cost
    tmp[visited] <- Inf

    idx <- which(
      tmp == min(tmp, na.rm = TRUE),
      arr.ind = TRUE
    )[1, ] %>%
      matrix(ncol = 2)

    nbs <- nb_idx(idx, nr, nc, visited)

    tentative_cost[nbs] <- pmin(
      cost[nbs] + tentative_cost[idx],
      tentative_cost[nbs],
      na.rm = TRUE
    )

    visited[idx] <- TRUE
  }
  tentative_cost[nr, nc]
}

# compute unvisited neighbor indices
nb_idx <- function(idx, nr, nc, visited) {
  indices <- cbind(
    idx[, 1] + c(-1, 0, 1, 0),
    idx[, 2] + c(0, 1, 0, -1)
  )
  indices <- indices[dplyr::between(indices[, 1], 1, nr), ]
  indices <- indices[dplyr::between(indices[, 2], 1, nc), ]
  indices <- indices[!visited[indices], ]

  matrix(indices, ncol = 2)
}

#' embiggen matrix for part 2
#'
#' @param file input file
#'
#' @export
embiggen_matrix <- function(file) {
  cost <- read_mat(file)

  nr <- nrow(cost)
  nc <- ncol(cost)

  big_cost <- matrix(nrow = 5*nr, ncol = 5*nc)

  value_dict <- rep(1:9, length.out = 20)

  for (i in 1:5) {
    for (j in 1:5) {
      start_row <- (i -1)*nr + 1 # i=1, 1. i=2, nr + 1, 1=3, 2*nr+1
      end_row <- start_row + nr - 1
      start_col <- (j - 1)*nc + 1
      end_col <- start_col + nc - 1

      big_cost[start_row:end_row, start_col:end_col] <- cost + i + j - 2

      # sub in values
      for (k in start_row:end_row) {
        for (l in start_col:end_col) {
          big_cost[k, l] <- value_dict[big_cost[k, l]]
        }
      }
    }
  }

  big_cost
}

