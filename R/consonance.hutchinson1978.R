consonance.hutchinson1978.uncached <- function(chord) {
  checkmate::assert_integerish(chord)

  ###################################################################################
  # this is the 'heavy lifting' for calculating brightness and consonance
  #
  tonic  = roughness(chord,observation_point=0)
  octave = roughness(chord,observation_point=12)
  # calculate 2-dimensional tonic-octave dissonance
  tonic_octave_dissonance = cbind(tonic,octave)
  # flip orientation to 2-dimensional tonic-octave consonance
  tonic_octave_consonance = consonance.hutchinson1978.max_dissonance() - tonic_octave_dissonance
  # rotate to 2-dimensional consonance-brightness
  consonance_brightness = tonic_octave_consonance %>% rotate(consonance.hutchinson1978.rotation_angle())

  # store the brightness consonance and dissonance
  tibble::tibble_row(
    tonic.dissonance  = tonic_octave_dissonance[1,1],
    octave.dissonance = tonic_octave_dissonance[1,2],
    tonic.consonance  = tonic_octave_consonance[1,1],
    octave.consonance = tonic_octave_consonance[1,2],
    consonance        = consonance_brightness[1,2],
    brightness        = consonance_brightness[1,1]
  )
}

#' Consonance:
#'
#' Implements Hutchinson's 1978 roughness metric of consonance
#'
#' @param chord A chord expressed as a vector of integers
#' @return A tibble
#'
#' @export
consonance.hutchinson1978 <- memoise::memoise(consonance.hutchinson1978.uncached)

roughness <- function(x,observation_point) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(observation_point,c(0,12))
  pitches = dplyr::bind_rows(c(x,observation_point) %>% unique %>% sort %>% purrr::map(pitch))
  spectrum = hrep::expand_harmonics(
    hrep::sparse_fr_spectrum(
      list(frequency = pitches$frequency,
           amplitude = rep(1,length(pitches$frequency)))))
  dycon::roughness_hutch(spectrum)
}

consonance.hutchinson1978.max_dissonance.uncached <- function() {
  # this is completely arbitrary
  # using the minor 2nd
  roughness(c(0,1),observation_point=0)
}
consonance.hutchinson1978.max_dissonance <- memoise::memoise(consonance.hutchinson1978.max_dissonance.uncached)

consonance.hutchinson1978.rotation_angle.uncached <- function() {
  max_diss = consonance.hutchinson1978.max_dissonance()
  rise = ((max_diss - roughness(0,0))  + (max_diss - roughness(0 ,12))) / 2
  run =  ((max_diss - roughness(0,12)) + (max_diss - roughness(12,12))) / 2
  pi / 2 - atan(rise / run)
}
consonance.hutchinson1978.rotation_angle <- memoise::memoise(consonance.hutchinson1978.rotation_angle.uncached)
