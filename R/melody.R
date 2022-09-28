#' Melody
#'
#' Provides the melodic metrics of a progression
#'
#'
#' @param progression A progression of chords as a multiple row tibble.
#' @param reference The reference harmony for calculating potential energy
#' @return A tibble
#'
#' @export
melody <- function(progression, reference=NULL) {
  checkmate::assert_list(progression,min.len=2)
  checkmate::assert_tibble(reference,null.ok=TRUE)
  progression_tibble = dplyr::bind_rows(progression)
  checkmate::assert_tibble(progression_tibble, min.cols=11, min.rows=2, any.missing = FALSE)
  if (is.null(reference)) {reference = progression[[1]]}
  # build the melody table
  t <- tibble::tibble(
    position_change         = c(0,(progression_tibble$position         %>% diff)),
    integer_position_change = c(0,(progression_tibble$integer_position %>% diff)),
    affinity_change         = c(0,(progression_tibble$affinity         %>% diff)),
    brightness_change       = c(0,(progression_tibble$brightness       %>% diff)),
    consonance_change       = c(0,(progression_tibble$consonance       %>% diff)),
    potential_energy        = potential_energy(progression, reference)
  )
  # store the original progression
  attr(t,"progression") <- progression
  attr(t,"reference") <- reference
  t
}

#' @rdname melody
#' @export
m <- melody

potential_energy <- function(progression,reference) {
  purrr::map(progression,function(x) {
    pitches = attr(x,"chord")
    reference_pitches = attr(reference,"chord")

    pe = tidyr::expand_grid(pitches,reference_pitches) %>% dplyr::rowwise() %>%
      dplyr::mutate(
        pe=energy(h(pitches,direction=x$direction,root=x$root),
                  h(reference_pitches,direction=reference$direction,root=reference$root)))
    pe$pe %>% unlist %>% mean %>% abs
  }) %>% unlist
}
##########
# energy
#
energy <- function(x,y) {
  abs(x$consonance-y$consonance)*(x$position-y$position)
}
