tone <- function(x) {
  checkmate::qassert(x,'X1')
  t = tibble::tibble(
    integer_position = x,
    tonic.tone       = frequency_ratio(x,'tonic.tone'),      # numerator
    tonic.ref        = frequency_ratio(x,'tonic.ref'),       # denominator
    tonic.primes     = prime_factors_sum(tonic.tone, tonic.ref),
    tonic.position   = 1200 * log2(tonic.tone  / tonic.ref), # cents
    octave.tone      = frequency_ratio(x,'octave.tone'),     # numerator
    octave.ref       = frequency_ratio(x,'octave.ref'),      # denominator
    octave.primes    = prime_factors_sum(octave.tone, octave.ref),
    octave.position  = 1200 * log2(octave.tone / octave.ref) # cents
  )
}

prime_factors_sum <- function(tone,ref) {
  checkmate::assert_integerish(tone,ref)

  c(numbers::primeFactors(tone), numbers::primeFactors(ref)) %>% purrr::map_dbl(
    ~numbers::primeFactors(.x)[numbers::primeFactors(.x)>1] %>% sum) %>%
    sum
}

frequency_ratio <- function(x,dimension) {
  checkmate::qassert(x,'X1')
  checkmate::assert_choice(dimension,c('tonic.tone','tonic.ref',
                                       'octave.tone','octave.ref'))

  if (x>=0 && x<=12) {
    (primary_frequency_ratios()[dimension] %>% unlist)[[x+1]]
  } else {
    integer = x %% 12
    octave_freq_multiplier = 2 ^ abs((x / 12) %>% floor)
    compound_ratio = (primary_frequency_ratios()[dimension] %>% unlist)[[integer+1]]

    if (x>12 && (dimension == 'tonic.tone' || dimension == 'octave.tone')) {
      compound_ratio * octave_freq_multiplier
    } else if (x<0 && (dimension == 'tonic.ref' || dimension == 'octave.ref')) {
      compound_ratio * octave_freq_multiplier
    } else {
      compound_ratio
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
