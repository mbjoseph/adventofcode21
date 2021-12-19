#' weird physics thing
#'
#' @param goal_x min and max of x goal region
#' @param goal_y min and max of y goal region
#'
#' @examples
#' goal_x <- c(137, 171)
#' goal_y <- c(-98, -73)
#' d <- find_maxima(goal_x, goal_y)
#' max(d$ymax, na.rm = TRUE) # 4753
#' sum(!is.na(d$ymax)) # 1546
#'
#' @export
find_maxima <- function(goal_x, goal_y) {
  tidyr::expand_grid(
    vx0 = 1:max(goal_x),
    vy0 = min(goal_y):200
  ) %>%
    mutate(ymax = purrr::map2_dbl(vx0, vy0, ~get_ymax(.x, .y)))
}



get_ymax <- function(vx0, vy0) {
  t_max <- 500
  t <- 1:t_max

  # max x position is reached when t == initial x velocity
  x <- t * (vx0 - (t - 1) / 2)
  x[(vx0 + 1):t_max] <- x[vx0]

  y <- t * (vy0 - (t - 1) / 2)

  hit <- any(
    dplyr::between(x, goal_x[1], goal_x[2]) &
      dplyr::between(y, goal_y[1], goal_y[2])
  )

  if (hit) {
    return(max(y))
  } else {
    return(NA)
  }
}
