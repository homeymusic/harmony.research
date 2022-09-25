# TODO: drop direction param and include up and down ratios in return value
frequency_ratio <- function(x,direction) {

  checkmate::qassert(x,'X1')
  checkmate::assert_choice(direction,c(-1,+1))

  probe.freq=ref.freq=probe.freq=ref.freq=NULL

  if (direction > 0) {
    probe.freq=frequency_ratios()$probe.freq.asc
    ref.freq=frequency_ratios()$ref.freq.tonic
  } else {
    probe.freq=frequency_ratios()$probe.freq.desc
    ref.freq=frequency_ratios()$ref.freq.octave
  }

  integer = x %% 12
  probe.freq=probe.freq[integer+1]
  ref.freq=ref.freq[integer+1]

  octave_freq_multiplier = 2 ^ abs((x / 12) %>% floor)
  if (x > 0) {
    probe.freq = probe.freq * octave_freq_multiplier
    # TODO: add 1,200 to integer_position for every octave
  } else if (x < 0) {
    ref.freq = ref.freq * octave_freq_multiplier
    # TODO: subtract  1,200 to integer_position for every octave
  }

  # handle the case where doubling the frequency, 1:2 becomes 2:2
  if(probe.freq==ref.freq){probe.freq=ref.freq=1}

  # TODO: include integer_position in cents
  # TODO: include up and down ratios in return value
  c(probe.freq=probe.freq,ref.freq=ref.freq)
}

frequency_ratios <- function() {
  tibble::tibble(

    # TODO: add integer_position column as cents

    #############################################
    # Tonic Frequency Ratios
    # probe frequency: ascending
    probe.freq.asc  = c(1,16,9,6,5,4,7,3,8,5,16,15,2), # numerator
    # reference frequency: tonic
    ref.freq.tonic  = c(1,15,8,5,4,3,5,2,5,3, 9, 8,1), # denominator

    #############################################
    # Octave Frequency Ratios
    # probe frequency: descending
    probe.freq.desc = c(1, 8, 9,3,5,2,5,3,4,5,8,15,1), # numerator
    # reference frequency: octave
    ref.freq.octave = c(2,15,16,5,8,3,7,4,5,6,9,16,1)  # denominator
  )
}
