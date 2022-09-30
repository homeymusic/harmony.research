pitch.uncached <- function(x) {
  checkmate::qassert(x,'X1')

  t <- tibble::tibble_row(
    integer_position = x,
    tonic.pitch      = compound_ratios(x,'tonic.pitch'),      # numerator
    tonic.ref        = compound_ratios(x,'tonic.ref'),       # denominator
    tonic.primes     = prime_factors_sum(.data$tonic.pitch, .data$tonic.ref),
    tonic.position   = 1200 * log2(.data$tonic.pitch  / .data$tonic.ref), # cents
    position         = .data$tonic.position,
    octave.pitch     = compound_ratios(x,'octave.pitch'),     # numerator
    octave.ref       = compound_ratios(x,'octave.ref'),      # denominator
    octave.primes    = prime_factors_sum(.data$octave.pitch, .data$octave.ref),
    octave.position  = 1200 * log2(.data$octave.pitch / .data$octave.ref) # cents
  )
  ###################################################################################
  # this is the 'heavy lifting' for calculating affinity, brightness and consonance
  #
  # calculate 2-dimensional tonic-octave dissonance
  tonic_octave_dissonance = cbind(t$tonic.primes,t$octave.primes)
  # flip orientation to 2-dimensional tonic-octave consonance
  tonic_octave_consonance = max_dissonance() - tonic_octave_dissonance
  # rotate pi/4 (45 deg) to 2-dimensional affinity-brightness
  affinity_brightness = tonic_octave_consonance %>% rotate(pi/4)

  # store the ABCDs: affinity brightness consonance dissonance
  t %>% tibble::add_column(
    tonic.dissonance  = tonic_octave_dissonance[1,1],
    octave.dissonance = tonic_octave_dissonance[1,2],
    tonic.consonance  = tonic_octave_consonance[1,1],
    octave.consonance = tonic_octave_consonance[1,2],
    affinity          = affinity_brightness[1,2],
    brightness        = affinity_brightness[1,1]
  )
}

#' Pitch
#'
#' Provides the metrics of a pitch
#'
#'
#' @param x A pitch expressed as an interval integer
#' @return A tibble
#'
#' @export
pitch <- memoise::memoise(pitch.uncached)

#' @rdname pitch
#' @export
p <- pitch

pitch_class_ratios <- function() {
  tonic.pitch  = c(1,16,9,6,5,4,7,3,8,5,16,15,2)
  tonic.ref    = c(1,15,8,5,4,3,5,2,5,3, 9, 8,1)

  list(
    #############################################
    # Tonic Frequency Ratios
    # pitch frequency: ascending
    tonic.pitch  = tonic.pitch, # numerator
    # reference frequency: tonic
    tonic.ref   = tonic.ref, # denominator

    #############################################
    # Octave Frequency Ratios
    # pitch frequency: descending
    octave.pitch = rev(tonic.ref), # numerator
    # reference frequency: octave
    octave.ref  = rev(tonic.pitch)  # denominator
  )
}

compound_ratios <- function(x,dimension) {
  checkmate::qassert(x,'X1')
  checkmate::assert_choice(dimension,c('tonic.pitch','tonic.ref',
                                       'octave.pitch','octave.ref'))

  if (x>=0 && x<=12) {
    # ratios are in the primary pitch class octave so all done
    (pitch_class_ratios()[dimension] %>% unlist)[[x+1]]
  } else {
    # else ratios are above or below the primary octave
    # start with the pitch class ratio
    integer = x %% 12
    pitch_class_ratio = (pitch_class_ratios()[dimension] %>% unlist)[[integer+1]]
    # calculate the octave adjustment
    octave_multiplier = 2 ^ abs((x / 12) %>% floor)

    if (x>12 && (dimension == 'tonic.pitch' || dimension == 'octave.pitch')) {
      # above the primary octave and the current dimension is the numerator
      # apply octave adjustment to the numerator
      pitch_class_ratio * octave_multiplier
    } else if (x<0 && (dimension == 'tonic.ref' || dimension == 'octave.ref')) {
      # below the primary octave and the current dimension is the denominator
      # apply octave adjustment to the denominator
      pitch_class_ratio * octave_multiplier
    } else {
      # the current dimension does not need to change
      pitch_class_ratio
    }
  }
}

prime_factors_sum <- function(pitch,ref) {
  checkmate::assert_integerish(pitch,ref)

  ratio = c(pitch,ref)

  ratio[ratio>1] %>% purrr::map(~numbers::primeFactors(.x)) %>% unlist %>% sum
}

# we are using the semitone, the minor second m2 from the tonic as max dissonance
# the result would be the same if we used major 7th M7 from octave perspective
max_dissonance <- function() {
  prime_factors_sum(pitch_class_ratios()$tonic.pitch[2],
                    pitch_class_ratios()$tonic.ref[2])
}

rotate <- function(coordinates,angle) {
  checkmate::assert_numeric(angle)
  coordinates = t(coordinates)
  R = tibble::frame_matrix(
    ~chord,          ~.y,
    cos(angle), -sin(angle),
    sin(angle),  cos(angle)
  )
  (R %*% coordinates * cos(angle)) %>% zapsmall %>% t
}
