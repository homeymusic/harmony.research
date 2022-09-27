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
  t = tibble::tibble()
  # store the original progression
  attr(t,"progression") <- progression

}

#' @rdname melody
#' @export
m <- melody
