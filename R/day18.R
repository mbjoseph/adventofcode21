#' get fish math magnitude
#'
#' @param file input file
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "day18.txt", package = "adventofcode21")
#' fishy_mag(file)
#' }
#'
#' @export
fishy_mag <- function(file) {
  lines <- readLines(file)
  d <- add(parse_fish(lines[1]), parse_fish(lines[2]))
  d <- reduce(d)
  for (i in 3:length(lines)) {
    d <- add(d, parse_fish(lines[i]))
    d <- reduce(d)
    print(i)
  }
  magnitude(d)
}



#' find maximum sum
#'
#' @param file input file
max_fishy_sum <- function(file) {
  lines <- readLines(file)

  future::plan(future::multisession)

  combos <- tidyr::expand_grid(
    x = lines,
    y = lines
  ) %>%
    filter(x != y) %>%
    mutate(
      sum = furrr::future_map2_dbl(
        x, y,
        ~ add(parse_fish(.x), parse_fish(.y)) %>%
          reduce() %>%
          magnitude() %>%
          pull(value),
        .progress = TRUE
      )
    )

  combos %>%
    slice_max(sum)
}



parse_fish <- function(x) {
  x %>%
    jsonlite::fromJSON(simplifyMatrix = FALSE, simplifyVector = FALSE) %>%
    reshape2::melt()
}

explode <- function(d) {
  levels <- parse_number(names(d)[-1])
  max_level <- max(levels)

  if (max_level < 5) {
    return(d)
  }

  deepest_level <- max_level %>% paste0("L", .)
  next_deepest_level <- (max_level - 1) %>% paste0("L", .)

  deepest_rows <- which(!is.na(d[[deepest_level]]))[1:2]

  # if it exists, add leftmost to left number
  if (deepest_rows[1] >= 1) {
    d$value[deepest_rows[1] - 1] <- sum(d$value[deepest_rows[1] - 1:0])
  }

  # same for right
  if (deepest_rows[2] < nrow(d)) {
    d$value[deepest_rows[2] + 1] <- sum(d$value[deepest_rows[2] + 0:1])
  }

  # replace with zero
  d$value[deepest_rows] <- 0
  d[[deepest_level]][deepest_rows] <- NA

  # if the deepest level is all NA, we can remove that column
  if (all(is.na(d[[deepest_level]]))) {
    d <- d[, !names(d) == deepest_level]
  }

  # we no longer need two rows for the original pair - just one (a 0)
  d[-deepest_rows[2], ]
}


fish_split <- function(d) {
  if (all(d$value < 10)) return(d)

  idx_to_split <- which(d$value > 9)[1]

  levels <- parse_number(names(d)[-1])

  # find depth of large number
  row_depths <- d[idx_to_split, names(d) %in% paste0("L", levels)] %>%
    unlist()

  new_depth <- which(!is.na(row_depths))[1] %>%
    names() %>%
    parse_number() %>%
    `+`(1)

  new_depth_colname <- paste0("L", new_depth)

  max_level <- max(levels)

  # if the new depth exceeds the max depth, we need a new column
  if (new_depth > max_level) {
    d[[new_depth_colname]] <- NA
  }

  half_val <- d$value[idx_to_split] / 2

  new_rows <- d[idx_to_split, ] %>%
    mutate(
      value = list(c(floor(half_val), ceiling(half_val)))
    ) %>%
    unnest(value)
  new_rows[[new_depth_colname]] <- 1:2

  top_df <- d[0:(idx_to_split - 1), ]
  if (idx_to_split != nrow(d)) {
    bottom_df <- d[(idx_to_split + 1):nrow(d), ]
  } else {
    bottom_df <- NULL
  }

  bind_rows(
    top_df,
    new_rows,
    bottom_df
  )
}


add <- function(x, y) {
  new_x <- increment_colnums(x)
  new_y <- increment_colnums(y)

  new_x$L1 <- 1
  new_y$L1 <- 2

  bind_rows(new_x, new_y)
}


increment_colnums <- function(d) {
  num_cols <- suppressWarnings(parse_number(names(d)))
  new_colnums <- num_cols + 1
  names(d) <- ifelse(is.na(new_colnums), names(d), paste0("L", new_colnums))
  d
}


reduce <- function(d) {
  while(TRUE) {
    numeric_cols <- names(d) != "value"
    # check for nesting in four pairs
    if (max(parse_number(names(d)[numeric_cols])) > 4) {
      d <- explode(d)
      next
    } else {
      no_nesting <- TRUE
    }

    # check for big numbers to split
    if (any(d$value > 9)) {
      d <- fish_split(d)
      next
    } else {
      no_big_nums <- TRUE
    }

    if (no_big_nums & no_nesting) {
      return(d)
    }
  }
}


magnitude <- function(d) {
  nesting_depths <- names(d)[names(d) != "value"] %>%
    parse_number() %>%
    sort(decreasing = TRUE)

  for (i in nesting_depths) {
    colname <- paste0("L", i)
    to_aggregate <- !is.na(d[[colname]])

    keep_rows <- d[!to_aggregate, ]

    new_rows <- d[to_aggregate, ] %>%
      group_by_at(vars(-value, -colname)) %>%
      arrange(!!as.symbol(colname)) %>%
      summarize(value = value[1] * 3 + value[2] * 2, .groups = "drop")

    d <- bind_rows(keep_rows, new_rows) %>%
      select(-colname)
  }
  d
}

