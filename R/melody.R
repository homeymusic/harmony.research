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
  checkmate::assert_tibble(progression_tibble, min.cols=11, min.rows=2)
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
    consonance_diff         = c(0,(progression_tibble$consonance       %>% diff)),
    potential_energy        = potential_energy(progression, reference),
    kinetic_energy          = c(0,kinetic_energy(progression)),
    kinetic_energy_velocity = c(0,kinetic_energy_velocity(progression))
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
        pe=energy(h(chord,root=reference$root),
                  h(reference_chord,root=reference$root)))
    pitch_crossings$pe %>% unlist %>% mean %>% abs
  })
}

kinetic_energy <- function(progression) {
  from = progression[-length(progression)]
  to   = progression[-1]

  purrr::map2_dbl(from,to,function(x,y) {
    from_chord = attr(x,"chord")
    to_chord   = attr(y,"chord")

    fewest_voices = min(length(from_chord),length(to_chord))
    from_chord = head(from_chord,fewest_voices)
    to_chord   = head(to_chord,fewest_voices)

    purrr::map2_dbl(from_chord,to_chord,function(.x,.y){
      energy(h(.x,root=x$root),
             h(.y,root=y$root))
    }) %>% abs %>% mean
  })
}
kinetic_energy_velocity <- function(progression) {
  from = progression[-length(progression)]
  to   = progression[-1]

  purrr::map2_dbl(from,to,function(x,y) {
    from_chord = attr(x,"chord")
    to_chord   = attr(y,"chord")

    fewest_voices = min(length(from_chord),length(to_chord))
    from_chord = head(from_chord,fewest_voices)
    to_chord   = head(to_chord,fewest_voices)

    purrr::map2_dbl(from_chord,to_chord,function(.x,.y){
      velocity_energy(h(.x,root=x$root),
             h(.y,root=y$root))
    }) %>% abs %>% mean
  })
}
##########
# energy
#
energy <- function(x,y) {
  (abs(x$affinity-y$affinity)+abs(x$brightness-y$brightness))*(x$integer_position-y$integer_position)
}
velocity_energy <- function(x,y) {
  0.5*abs(x$consonance-y$consonance)*(x$integer_position-y$integer_position)^2
}
