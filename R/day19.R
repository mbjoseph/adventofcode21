#' Scanner flippin
#'
#' @param input file
#' @examples
#' file <- system.file("extdata", "day19.txt", package = "adventofcode21")
#' transmogrify_scanners(file)
#'
#' @export
transmogrify_scanners <- function(file) {

  data <- readLines("inst/extdata/day19.txt") %>%
    tibble(x = .) %>%
    mutate(scanner = cumsum(grepl("scanner", x)) - 1) %>%
    filter(!grepl("scanner", x), x!= "") %>%
    separate(x, into = c("x", "y", "z"), sep = ",") %>%
    mutate_all(as.integer)


  scanner_crosswalk <- tidyr::expand_grid(
    ref_scanner = unique(data$scanner),
    unk_scanner = unique(data$scanner)
  ) %>%
    filter(ref_scanner != unk_scanner, unk_scanner != 0) %>%
    mutate(matches = purrr::map2(ref_scanner, unk_scanner, ~match_beacons(.x, .y, data))) %>%
    filter(!purrr::map_lgl(matches, is.null)) %>%
    mutate(transform = purrr::map(matches, get_transform))

  conversion_graph <- scanner_crosswalk %>%
    rename(to = ref_scanner,
           from = unk_scanner) %>%
    tidygraph::as_tbl_graph()

  plot(conversion_graph)

  # find the shortest path to convert each scanner to the coords of scanner 0
  shortest_paths <- tibble(
    scanner = unique(data$scanner[data$scanner != 0])
  ) %>%
    mutate(
      path = purrr::map(
        scanner,
        ~{
          igraph::shortest_paths(
            conversion_graph,
            from = as.character(.x),
            to = "0"
          )$vpath
        }
      ),
      node_sequence = purrr::map(path, ~unlist(.x) %>% names())
    ) %>%
    select(-path)


  # execute a series of coordinate transformations along the path

  sequential_transform <- function(coords, node_sequence) {
    node_sequence <- as.numeric(node_sequence)

    for (i in 2:length(node_sequence)) {
      # get transform object
      cw <- scanner_crosswalk %>%
        filter(
          ref_scanner == node_sequence[i], unk_scanner == node_sequence[i-1]
        )

      # apply the transformation
      new_coords <- tibble(
        x = round(predict(cw$transform[[1]]$mx, newdata = coords)),
        y = round(predict(cw$transform[[1]]$my, newdata = coords)),
        z = round(predict(cw$transform[[1]]$mz, newdata = coords))
      )
      coords <- new_coords
    }

    coords
  }


  corrected_coords <- data %>%
    filter(scanner != 0) %>%
    group_by(scanner) %>%
    nest(coords = c(x, y, z)) %>%
    left_join(shortest_paths) %>%
    mutate(new_coords = purrr::map2(coords, node_sequence, sequential_transform)) %>%
    select(scanner, new_coords) %>%
    unnest(new_coords) %>%
    ungroup() %>%
    distinct(x, y, z)



  final_beacon_coords <- data %>%
    filter(scanner == 0) %>%
    select(-scanner) %>%
    full_join(corrected_coords)

  # need scanner locations
  # in their own coords, each scanner is at the origin
  scanner_coords <- tidyr::expand_grid(
    scanner = 1:max(data$scanner),
    x = 0,
    y = 0,
    z = 0
  )

  # follow the sequence of transformations to get into coords of scanner 0
  corrected_scanner_coords <- scanner_coords %>%
    group_by(scanner) %>%
    nest(coords = c(x, y, z)) %>%
    left_join(shortest_paths) %>%
    mutate(new_coords = purrr::map2(coords, node_sequence, sequential_transform)) %>%
    select(scanner, new_coords) %>%
    unnest(new_coords) %>%
    ungroup()

  scanner_0 <- tibble(scanner = 0, x = 0, y = 0, z = 0)

  max_manhattan_dist <- corrected_scanner_coords %>%
    bind_rows(scanner_0) %>%
    select(x, y, z) %>%
    mutate_all(as.integer) %>%
    dist(method = "manhattan") %>%
    max()

  list(
    part1 = nrow(final_beacon_coords),
    part2 =  max_manhattan_dist
  )
}



# find matches based on distance
match_beacons <- function(ref_scanner, unk_scanner, data) {

  ref_data <- data %>%
    filter(scanner == ref_scanner) %>%
    select(x, y, z)

  tmp_data <- data %>%
    filter(scanner == unk_scanner) %>%
    select(x, y, z)

  ref_dists <- ref_data %>%
    dist() %>%
    as.matrix()

  tmp_dists <- tmp_data %>%
    dist() %>%
    as.matrix()

  shared_dists <- intersect(tmp_dists, ref_dists) %>%
    sort()
  shared_dists <- shared_dists[shared_dists != 0]


  ref_indices <- purrr::map(
    shared_dists,
    ~which(ref_dists == .x, arr.ind = TRUE)[1, ] # 1 or 2, order no matter
  ) %>%
    bind_rows()

  ref_order <- unique(c(ref_indices$row, ref_indices$col ))

  ref_subset <- ref_data[ref_order, ]

  # find permutation of tmp data such that distance matrix matches reference
  tmp_indices <- purrr::map(
    shared_dists,
    ~which(tmp_dists == .x, arr.ind = TRUE)[1, ]
  ) %>%
    bind_rows()

  tmp_order <- sort(unique(c(tmp_indices$row, tmp_indices$col)))

  tmp_subset <- tmp_data[tmp_order, ]

  if (nrow(tmp_subset) < 12) {
    return(NULL)
  }

  # use PCA to find the ordering that provides identical distance matrices
  tmp_pc <- princomp(tmp_subset)
  ref_pc <- princomp(ref_subset)

  tmp_rot <- abs(tmp_pc$scores) %>% round() %>% apply(1, paste, collapse="_")
  ref_rot <- abs(ref_pc$scores) %>% round() %>% apply(1, paste, collapse="_")

  index_matches <- match(ref_rot, tmp_rot)

  if (any(is.na(index_matches))) browser()

  ordered_tmp_subset <- tmp_subset[index_matches, ]

  # check whether we've sorted correctly -- distance matrices about equal
  if (sum(abs(dist(ordered_tmp_subset) - dist(ref_subset))) > 1e-3) {
    browser()
  }

  return(
    list(
      ref_subset = ref_subset,
      unk_subset = ordered_tmp_subset
    )
  )
}




get_transform <- function(matches) {

  ref_subset <- matches$ref_subset
  unk_subset <- matches$unk_subset

  x_0 <- ref_subset$x
  y_0 <- ref_subset$y
  z_0 <- ref_subset$z

  mx <- lm(x_0 ~ x + y + z, data = unk_subset)
  my <- lm(y_0 ~ x + y + z, data = unk_subset)
  mz <- lm(z_0 ~ x + y + z, data = unk_subset)

  mx$coefficients <- round(mx$coefficients)
  my$coefficients <- round(my$coefficients)
  mz$coefficients <- round(mz$coefficients)

  list(mx = mx, my = my, mz = mz)
}



