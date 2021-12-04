#' part 1
#'
#' @param f input file
#' @param goal win or lose
#'
#' @importFrom stats na.omit
#' @importFrom rlang .data
#'
#' @examples
#' f <- system.file("extdata", "day4.txt", package = "adventofcode21")
#' pick_board(f, goal = "win") # 63552
#' pick_board(f, goal = "lose") # 9020
#'
#' @export
pick_board <- function(f, goal = c("win", "lose")) {
  draws <- readLines(f, n = 1) %>%
    strsplit(split = ",") %>%
    unlist() %>%
    as.numeric()

  boards <- readr::read_fwf(f, skip = 1) %>%
    na.omit() %>%
    dplyr::mutate(
      board = rep(
        1:(dplyr::n() / 5),
        each = 5
      ),
      row = rep(1:5, times = max(.data$board))
    ) %>%
    tidyr::pivot_longer(
      tidyselect::starts_with("X")
    ) %>%
    dplyr::transmute(
      .data$board, .data$row, col = readr::parse_number(.data$name), .data$value
    )

  win_list <- list()

  for (i in draws) {
    boards <- boards %>%
      dplyr::mutate(value = ifelse(.data$value == i, NA, .data$value))

    rows <- boards %>%
      dplyr::group_by(.data$board, .data$row) %>%
      dplyr::filter(all(is.na(.data$value)))

    cols <- boards %>%
      dplyr::group_by(.data$board, .data$col) %>%
      dplyr::filter(all(is.na(.data$value)))

    potential_winner <- dplyr::bind_rows(rows, cols) %>%
      dplyr::ungroup()

    if (nrow(potential_winner) > 0) {
      winning_board <- boards %>%
        dplyr::filter(.data$board %in% potential_winner$board)

      win_list <- append(win_list, list(list(board = winning_board, num = i)))

      if (goal == "win") {
        return(sum(winning_board$value, na.rm = TRUE) * i)
      }

      if (goal == "lose") {
        boards <- boards %>%
          dplyr::filter(!.data$board %in% potential_winner$board)
      }
    }
  }

  final_winner <- win_list[[length(win_list)]]
  sum(final_winner$board$value, na.rm = TRUE) * final_winner$num
}
