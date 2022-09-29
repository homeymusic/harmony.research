#' Pitch
#'
#' Provides the metrics of a pitch
#'
#'
#' @param x A pitch expressed as an interval integer
#' @return A tibble
#'
#' @export
pitch <- function(x) {
  checkmate::qassert(x,'X1')
  t <- tibble::tibble_row(
    integer_position = x,
    tonic.pitch       = frequency_ratio(x,'tonic.pitch'),      # numerator
    tonic.ref        = frequency_ratio(x,'tonic.ref'),       # denominator
    tonic.primes     = prime_factors_sum(.data$tonic.pitch, .data$tonic.ref),
    tonic.position   = 1200 * log2(.data$tonic.pitch  / .data$tonic.ref), # cents
    octave.pitch      = frequency_ratio(x,'octave.pitch'),     # numerator
    octave.ref       = frequency_ratio(x,'octave.ref'),      # denominator
    octave.primes    = prime_factors_sum(.data$octave.pitch, .data$octave.ref),
    octave.position  = 1200 * log2(.data$octave.pitch / .data$octave.ref) # cents
  )
}

#' @rdname pitch
#' @export
p <- pitch

prime_factors_sum <- function(pitch,ref) {
  checkmate::assert_integerish(pitch,ref)

  ratio = c(pitch,ref)

  ratio[ratio>1] %>% purrr::map(~numbers::primeFactors(.x)) %>% unlist %>% sum
}

frequency_ratio <- function(x,dimension) {
  checkmate::qassert(x,'X1')
  checkmate::assert_choice(dimension,c('tonic.pitch','tonic.ref',
                                       'octave.pitch','octave.ref'))

  if (x>=0 && x<=12) {
    # ratios in the primary octave
    (primary_frequency_ratios()[dimension] %>% unlist)[[x+1]]
  } else {
    # ratios above or below the primary octave
    integer = x %% 12
    octave_freq_multiplier = 2 ^ abs((x / 12) %>% floor)
    compound_freq = (primary_frequency_ratios()[dimension] %>% unlist)[[integer+1]]

    if (x>12 && (dimension == 'tonic.pitch' || dimension == 'octave.pitch')) {
      # above the primary octave and the current dimension is the numerator
      compound_freq * octave_freq_multiplier
    } else if (x<0 && (dimension == 'tonic.ref' || dimension == 'octave.ref')) {
      # below the primary octave and the current dimension is the denominator
      compound_freq * octave_freq_multiplier
    } else {
      # the current dimension does not need to change
      compound_freq
    }
  }
}

primary_frequency_ratios <- function() {
  tonic.pitch  = c(1,16,9,6,5,4,7,3,8,5,16,15,2)
  tonic.ref   = c(1,15,8,5,4,3,5,2,5,3, 9, 8,1)

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
