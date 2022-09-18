frequency_ratios.uncached <- function() {
  tibble::tibble(
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
frequency_ratios <- memoise::memoise(frequency_ratios.uncached)

frequency_ratio <- function(x,direction) {
  # print('interval passed into frequency_ratio')
  # print(x)
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

  interval = x %% 12
  probe.freq=probe.freq[interval+1]
  ref.freq=ref.freq[interval+1]

  octave_multiplier = 2 ^ abs((x / 12) %>% floor)
  if (x > 0) {
    probe.freq = probe.freq * octave_multiplier
  } else if (x < 0) {
    ref.freq = ref.freq * octave_multiplier
  }

# handle the case where doubling the frequency
    if(probe.freq==ref.freq){probe.freq=ref.freq=1}

  c(probe.freq=probe.freq,ref.freq=ref.freq)
}
# ah the tritone, what a pain
#
# each octave complement is symmetrical wrt to ratios and distance in cents
# ideally we would have two tritones or use the ET tritone of 600 cents, sqrt(2)
# but to keep the code cleaner we ought to choose a tritone with the smallest
# prime factors but close enough to 600 cents so that it is below human perception.
#
# best choice with that rationale would be 17/12 and 24/17, 3.000 cents from 600
# however the consonance measure is poor that it doesn't agree with any published
# experimental data that we could find cf. Bowling, Plomp and Levitt and others
# like https://www.jneurosci.org/content/29/42/13165
#
# The tritone that seems to best match the research is 7/5 10/7 however its
# a full 17.488 cents from 600 which means it's 3x perceptibly different from
# the ET tritone.
#
# alas, given two bad choices we choose to match the empirical data.
#
# see:
# https://en.wikipedia.org/wiki/Cent_(music)#Human_perception
# https://enxen.wiki/w/Tritone
#
# another issue is that the final affinity (rotated consonance) of all the
# other intervals is the average consonance of the octave complements
# so the tritone ought to be the average of 7/5 and 10/7
# so we hammer that into the final function for dissonance
