#' grow a population
#'
#' @param init initial age vector
#' @param t number of time steps
#'
#' @examples
#' init <- scan(system.file("extdata", "day6.txt", package = "adventofcode21"),
#'              sep = ",")
#' grow(init, t = 80)
#' grow(init, t = 256)
#'
#' @importFrom tibble tibble
#' @importFrom dplyr count lead mutate
#' @importFrom tidyr complete
#' @export
grow <- function(init, t) {
  state <- tibble(days_left = init) %>%
    count(days_left) %>%
    complete(days_left = 0:8, fill = list(n = 0))

  for (i in 1:t) {
    state <- state %>%
      mutate(
        n = lead(n, default = 0) +    # everyone gets old
          c(rep(0, 8), n[1]) +         # some have bebes
          c(rep(0, 6), n[1], c(0, 0))  # reset reproducers
      )
  }
  sum(state$n)
}
