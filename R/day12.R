#' Find all paths
#'
#' @param file input file
#'
#' @importFrom dplyr case_when
#' @importFrom tidyr separate
#' @importFrom readr read_csv2
#' @importFrom stringr str_count str_detect str_replace
#'
#' @export
find_all_paths <- function(file) {

  usable_edges <- get_usable_edges(file)

  # initialize starting paths
  paths <- usable_edges %>%
    dplyr::filter(start == "start") %>%
    tidyr::unite(path, start, end, sep = "-")

  # while not all paths have an end, grow paths in all possible valid directions
  while(!all(grepl("end$", paths$path))) {

    finished <- paths %>%
      dplyr::filter(grepl("end$", path))

    paths <- paths %>%
      dplyr::filter(!grepl("end$", path)) %>%
      dplyr::mutate(start = str_replace(path, ".*-", "")) %>%
      dplyr::left_join(usable_edges) %>%
      dplyr::mutate(end = ifelse(is.na(end), "", end)) %>%
      dplyr::mutate(
        end_lower = str_detect(end, "[[:lower:]]") &  !end %in% c("start", "end"),
        end_visited = str_detect(path, paste0("-", end, "$|-", end, "-"))
      ) %>%
      dplyr::filter(
        !(end_lower & end_visited)
      ) %>%
      dplyr::mutate(
        path = paste(path, end, sep = "-"),
        path = str_replace(path, "-$", "")
      ) %>%
      dplyr::select(path) %>%
      dplyr::bind_rows(finished)
  }
  nrow(paths)
}



#' part 2: find all paths, allowing one "twofer"
#'
#' @param file input file
#'
#' @export
find_twofer_paths <- function(file) {

  usable_edges <- get_usable_edges(file)

  paths <- usable_edges %>%
    dplyr::filter(start == "start") %>%
    tidyr::unite(path, start, end, sep = "-") %>%
    dplyr::mutate(twofer_used = FALSE)


  while(!all(grepl("end$", paths$path))) {

    finished <- paths %>%
      dplyr::filter(grepl("end$", path))

    paths <- paths %>%
      dplyr::filter(!grepl("end$", path)) %>%
      dplyr::mutate(start = str_replace(path, ".*-", "")) %>%
      dplyr::left_join(usable_edges) %>%
      dplyr::mutate(
        end = ifelse(is.na(end), "", end),
        end_lower = str_detect(end, "[[:lower:]]") &  !end %in% c("start", "end"),
        times_end_visited = dplyr::if_else(end == "", 0L, str_count(path, end))
      ) %>%
      dplyr::filter(
        case_when(
          twofer_used ~ !(end_lower & times_end_visited > 0),
          TRUE ~ TRUE
        )
      ) %>%
      dplyr::mutate(
        twofer_used = case_when(
          twofer_used ~ TRUE,
          end_lower & times_end_visited > 0 ~ TRUE,
          TRUE ~ FALSE
        ),
        path = paste(path, end, sep = "-"),
        path = str_replace(path, "-$", "")
      ) %>%
      dplyr::select(path, twofer_used) %>%
      dplyr::bind_rows(finished)
  }
  nrow(paths)
}


get_usable_edges <- function(file) {
  directed_edges <- read_csv2(file, col_names = FALSE) %>%
    separate(X1, into = c("start", "end"), sep = "-")

  rev_edges <- tibble(
    start = directed_edges$end,
    end = directed_edges$start
  )

  dplyr::bind_rows(directed_edges, rev_edges) %>%
    dplyr::filter(start != "end", end != "start") %>% # absorbing states
    dplyr::mutate(
      is_deadend = !end %in% start,
      lower_start = str_detect(start,"[[:lower:]]") & !start %in% c("start", "end"),
      lower_end = str_detect(end, "[[:lower:]]") & !end %in% c("start", "end")
    ) %>%
    dplyr::filter(!(is_deadend & lower_start & lower_end)) %>%
    dplyr::select(start, end)
}
