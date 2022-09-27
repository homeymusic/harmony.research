tone <- function(x) {
  checkmate::qassert(x,'X1')
  t = tibble::tibble(
    integer_position = x,
    tonic.tone       = frequency_ratio(x,'tonic.tone'),      # numerator
    tonic.ref        = frequency_ratio(x,'tonic.ref'),       # denominator
    tonic.primes     = prime_factors_sum(.data$tonic.tone, .data$tonic.ref),
    tonic.position   = 1200 * log2(.data$tonic.tone  / .data$tonic.ref), # cents
    octave.tone      = frequency_ratio(x,'octave.tone'),     # numerator
    octave.ref       = frequency_ratio(x,'octave.ref'),      # denominator
    octave.primes    = prime_factors_sum(.data$octave.tone, .data$octave.ref),
    octave.position  = 1200 * log2(.data$octave.tone / .data$octave.ref) # cents
  )
}

prime_factors_sum <- function(tone,ref) {
  checkmate::assert_integerish(tone,ref)

  ratio = c(tone,ref)

  ratio[ratio>1] %>% purrr::map(~numbers::primeFactors(.x)) %>% unlist %>% sum
}

frequency_ratio <- function(x,dimension) {
  checkmate::qassert(x,'X1')
  checkmate::assert_choice(dimension,c('tonic.tone','tonic.ref',
                                       'octave.tone','octave.ref'))

  if (x>=0 && x<=12) {
    # ratios in the primary octave
    (primary_frequency_ratios()[dimension] %>% unlist)[[x+1]]
  } else {
    # ratios above or below the primary octave
    integer = x %% 12
    octave_freq_multiplier = 2 ^ abs((x / 12) %>% floor)
    compound_freq = (primary_frequency_ratios()[dimension] %>% unlist)[[integer+1]]

    if (x>12 && (dimension == 'tonic.tone' || dimension == 'octave.tone')) {
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
  tonic.tone  = c(1,16,9,6,5,4,7,3,8,5,16,15,2)
  tonic.ref   = c(1,15,8,5,4,3,5,2,5,3, 9, 8,1)

  list(
    #############################################
    # Tonic Frequency Ratios
    # tone frequency: ascending
    tonic.tone  = tonic.tone, # numerator
    # reference frequency: tonic
    tonic.ref   = tonic.ref, # denominator

    #############################################
    # Octave Frequency Ratios
    # tone frequency: descending
    octave.tone = rev(tonic.ref), # numerator
    # reference frequency: octave
    octave.ref  = rev(tonic.tone)  # denominator
  )
}
