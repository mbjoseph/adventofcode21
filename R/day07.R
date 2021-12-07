#' Moving crabbies on the cheap
#'
#' @param init initial positions
#'
#' @examples
#' init <- scan(system.file("extdata", "day7.txt", package = "adventofcode21"),
#'              sep = ",")
#' least_cost(init) # 344138
#' fancy_least_cost(init) # 94862124
#'
#' @export
least_cost <- function(init) {
  solns <- seq(min(init), max(init))
  abs_dist_matrix <- abs(outer(init, solns, FUN = "-"))  # (n_crab, n_solns)
  min(colSums(abs_dist_matrix))
}

#' Fancy boi
#'
#' @param init initial positions
#'
#' @export
fancy_least_cost <- function(init) {
  solns <- seq(min(init), max(init))
  abs_dist_matrix <- abs(outer(init, solns, FUN = "-")) # (n_crab, n_solns)
  cost_matrix <- abs_dist_matrix * (abs_dist_matrix + 1) / 2
  min(colSums(cost_matrix))
}
