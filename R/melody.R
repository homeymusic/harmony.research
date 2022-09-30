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

  t <- tibble::tibble(
    integer_name     = pe_integer_name(progression,reference),
    potential_energy = potential_energy(progression, reference),
    kinetic_energy   = kinetic_energy(progression, reference)
  )

  # store the reference harmony
  attr(t,"reference") <- reference
  # store the original progression
  colnames(progression_tibble) = paste0(".", colnames(progression_tibble))
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

  from = purrr::prepend(progression[-length(progression)],list(reference))
  to   = progression

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
  x$position-y$position
}

pe_integer_name <- function(progression,reference) {
  from = purrr::prepend(progression[-length(progression)],list(reference))
  to   = progression
  purrr::map2_chr(from,to,function(x,y){
    paste(paste(x$integer_name, y$integer_name, sep =' \u21D2 '),
          paste0('(',reference$integer_name,')'))

  })
}
