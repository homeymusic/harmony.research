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
# https://en.xen.wiki/w/Tritone
#
# another issue is that the final affinity (rotated consonance) of all the
# other intervals is the average consonance of the octave complements
# so the tritone ought to be the average of 7/5 and 10/7
# so we hammer that into the final function for dissonance

ratios <- tibble::tibble(
  # Up ratios
  up.numerator =     c(1,16,9,6,5,4,7,3,8,5,16,15,2),
  up.denominator =   c(1,15,8,5,4,3,5,2,5,3, 9, 8,1),
  # Down ratios
  down.numerator =   rev(.data$up.denominator),
  down.denominator = rev(.data$up.numerator)
)
