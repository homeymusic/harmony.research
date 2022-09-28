#' Melody
#'
#' Provides the melodic metrics of a progression
#'
#'
#' @param progression A progression of chords as a multiple row tibble.
#' @return A tibble
#'
#' @export
melody <- function(progression) {
  checkmate::assert_list(progression,min.len=2)
  progression_tibble = dplyr::bind_rows(progression)
  checkmate::assert_tibble(progression_tibble, min.cols=11, min.rows=2, any.missing = FALSE)
  # build the melody table
  t <- tibble::tibble(
    position_change         = progression_tibble$position         %>% diff,
    integer_position_change = progression_tibble$integer_position %>% diff,
    affinity_change         = progression_tibble$affinity         %>% diff,
    brightness_change       = progression_tibble$brightness       %>% diff,
    consonance_change       = progression_tibble$consonance       %>% diff,
    potential_energy        = progression                         %>% potential_energy
  )
  # store the original progression
  attr(t,"progression") <- progression
  t
}

#' @rdname melody
#' @export
m <- melody

potential_energy <- function(progression) {
  # print(progression[[2]] %>% attributes)
  0
}
