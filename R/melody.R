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
  checkmate::assert_tibble(progression, min.cols=11, min.rows=2, any.missing = FALSE)
  # build the melody table
  t <- tibble::tibble(
    distance = progression$position %>% diff,
    integer_distance = progression$integer_position %>% diff,
    affinity_change = progression$affinity %>% diff,
    brightness_change = progression$brightness %>% diff,
    consonance_change = progression$consonance %>% diff
  )
  # store the original progression
  attr(t,"progression") <- progression

  t
}

#' @rdname melody
#' @export
m <- melody
