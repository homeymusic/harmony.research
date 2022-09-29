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
  checkmate::assert_tibble(progression_tibble, min.cols=9, min.rows=2)
  if (is.null(reference)) {reference = progression[[1]]}
  # build the melody table
  # TODO:    include progression integer name for each row.
  # integer_name            = paste(paste(progression_tibble$integer_name,
  # collapse =' \u21D2 '),
  # paste0('(',reference$integer_name,')')),
  t <- tibble::tibble(
    position_diff           = c(0,(progression_tibble$position         %>% diff)),
    integer_position_diff   = c(0,(progression_tibble$integer_position %>% diff)),
    affinity_diff           = c(0,(progression_tibble$affinity         %>% diff)),
    brightness_diff         = c(0,(progression_tibble$brightness       %>% diff)),
    potential_energy        = potential_energy(progression, reference),
    kinetic_energy          = c(0,kinetic_energy(progression, reference))
  )
  # store the reference harmony
  attr(t,"reference") <- reference
  # store the original progression
  dplyr::bind_cols(t,progression_tibble)
}

#' @rdname melody
#' @export
m <- melody

potential_energy <- function(progression,reference) {
  purrr::map_dbl(progression,function(x) {
    chord = attr(x,"chord")
    reference_chord = attr(reference,"chord")
    pitch_crossings = tidyr::expand_grid(chord,reference_chord) %>% dplyr::rowwise() %>%
      dplyr::mutate(
        pe=energy(pitch(chord,root=reference$root),
                  pitch(reference_chord,root=reference$root)))
    pitch_crossings$pe %>% unlist %>% mean %>% abs
  })
}

kinetic_energy <- function(progression,reference) {
  from = progression[-length(progression)]
  to   = progression[-1]

  purrr::map2_dbl(from,to,function(x,y) {
    from_chord = attr(x,"chord")
    to_chord   = attr(y,"chord")

    fewest_voices = min(length(from_chord),length(to_chord))
    from_chord = head(from_chord,fewest_voices)
    to_chord   = head(to_chord,fewest_voices)

    purrr::map2_dbl(from_chord,to_chord,function(.x,.y){
      energy(pitch(.x,root=reference$root),
             pitch(.y,root=reference$root))
    }) %>% abs %>% mean
  })
}

energy <- function(x,y) {
  force(x,y)*distance(x,y)
}
force <- function(x,y) {
  abs(x$affinity-y$affinity)+abs(x$brightness-y$brightness)
}
# TODO: account for changes in chord duration, tempo, etc
# right now we are assuming 60 bpm and each chord gets one beat
distance <- function(x,y) {
  x$integer_position-y$integer_position
}
