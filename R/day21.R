library(tidyverse)



# Part 1 ------------------------------------------------------------------

pos <- c(3, 7)
score <- c(0, 0)
turn <- 1

nseq <- rep(1:100, times = 30)
sums <- matrix(nseq, ncol = 3, byrow = TRUE) %>%
  rowSums()

idx <- 1

while(max(score) < 1000) {
  die_sum <- sums[idx]
  idx <- idx + 1

  pos[turn] <- (pos[turn] + die_sum) %>%
    as.character() %>%
    stringr::str_sub(start = -1) %>%
    as.numeric()

  if (pos[turn] == "0") {
    score[turn] <- score[turn] + 10
  } else {
    score[turn] <- score[turn] + as.numeric(pos[turn])
  }

  if (turn == 1) {
    turn <- 2
  } else {
    turn <- 1
  }

  print(die_sum)
  print(pos)
  print(score)

}

(idx * 3 - 3) * min(score)



# part 2
rolls <- tidyr::expand_grid(
  roll1 = 1:3,
  roll2 = 1:3,
  roll3 = 1:3
) %>%
  rowSums() %>%
  tibble(dice_sum = .) %>%
  count(dice_sum)

step <- function(d) {
  if (turn == 1) {
    new_pos <- (d$p1_position + rolls$dice_sum) %% 10
    new_pts <- d$p1_points + ifelse(new_pos == 0, 10, new_pos)

    return(
      tibble(
        p1_position = new_pos,
        p2_position = d$p2_position,
        p1_points = new_pts,
        p2_points = d$p2_points,
        n = d$n * rolls$n
      )
    )

  } else {
    new_pos <- (d$p2_position + rolls$dice_sum) %% 10
    new_pts <- d$p2_points + ifelse(new_pos == 0, 10, new_pos)

    return(
      tibble(
        p1_position = d$p1_position,
        p2_position = new_pos,
        p1_points = d$p1_points,
        p2_points = new_pts,
        n = d$n * rolls$n
      )
    )
  }
}


library(furrr)
library(future)

plan(multisession, workers = 4)

states <- tibble(
  p1_position = 3,
  p2_position = 7,
  p1_points = 0,
  p2_points = 0,
  n = 1
)

completed <- tibble()

while(nrow(states) > 0) {
  for (turn in 1:2) {
    states <- states %>%
      mutate(id = row_number()) %>%
      split(.$id) %>%
      furrr::future_map(~step(.x), .progress = TRUE) %>%
      bind_rows() %>%
      count(p1_position, p2_position, p1_points, p2_points, wt = n)

    print(sum(states$n))

    newly_completed <- filter(states, pmax(p1_points, p2_points) >= 21)

    completed <- bind_rows(completed, newly_completed) %>%
      count(p1_position, p2_position, p1_points, p2_points, wt = n)

    states <- states %>%
      filter(pmax(p1_points, p2_points) < 21)
  }
}

completed %>%
  mutate(p1_wins = p1_points > p2_points) %>%
  group_by(p1_wins) %>%
  summarize(n = format(sum(n), digits = 16))


ggplot(completed, aes(p1_position, p2_position)) +
  geom_jitter(aes(size=n, color = p1_points - p2_points), width = 0.5, height = 0.5) +
  scale_color_gradient2("Score differential") +
  theme_dark() +
  xlab("Player 1 end position") +
  ylab("Player 2 end position")
