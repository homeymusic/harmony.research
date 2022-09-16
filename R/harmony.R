# this is to stop R check command from complaining about magrittr
globalVariables(".")

harmony.uncached <- function(.x, .direction=+1, .reference_tone=NULL, .name=NULL) {
  checkmate::assert_integerish(.x)
  checkmate::assert_choice(.direction,c(-1,+1))

  .reference_tone = ifelse(is.null(.reference_tone),
                           reference_tone(.x,.direction),
                           .reference_tone)

  .consonance = consonance(.x,.direction,.reference_tone)

  t = tibble::tibble(
    position =         .x %>% mean,
    direction =        .direction,
    reference_tone =   .reference_tone,
    name =             .name,
    intervallic_name =  intervallic_name(.x,.direction,.reference_tone),
    affinity =         .consonance[['affinity']],
    brightness =       .consonance[['brightness']]
  )
  attr(t,"chord") <- .x
  t
}

#' Harmony
#'
#' Provides the musical harmony metrics of a note or chord.
#'
#'
#' @param .x A note or chord expressed as an interval integer or vector of interval integers
#' @param .direction Harmonic direction +1 is up and -1 is down
#' @param .reference_tone The reference tone of the chord or larger context
#' @param .name A custom name for the note or chord
#' @return A tibble
#'
#' @export
harmony <- memoise::memoise(harmony.uncached)

#' @rdname harmony
#' @export
h <- harmony

consonance <- function(x,direction,reference_tone) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(direction,c(-1,+1))
  checkmate::assert_integerish(reference_tone)


  c('affinity'=0,'brightness'=0)
}

reference_tone <- function(x,direction) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(direction,c(-1,+1))
  ifelse(direction>0,min(x),max(x))
}

intervallic_name <- function(x, direction, reference_tone) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(direction,c(-1,+1))
  checkmate::assert_integerish(reference_tone)

  underline =  '\u0332' # 0̲ underlines the character preceding the unicode
  up_arrow =   '\u21D1' # ⇑
  down_arrow = '\u21D3' # ⇓

  underlined_reference_tone = stringr::str_replace_all(reference_tone,
                                                       "(.)",
                                                       paste0("\\1",underline))
  intervallic_name = x %>% paste(collapse = ":")
  if (reference_tone %in% x) {
    intervallic_name=gsub(paste0("\\b",reference_tone,"\\b"),
                          underlined_reference_tone,intervallic_name)
  } else {
    intervallic_name = paste(underlined_reference_tone,intervallic_name)
  }
  paste0(intervallic_name, ifelse (direction == 1, up_arrow,down_arrow))
}

# from Leonhard Euler’s Tentamen Novae Theoriae Musicae:
#      A Translation and Commentary
#      by Charles Samuel Smith
#
# p. 103 Chapter IV sec 6
# "Henceforth the term exponent will be used to designate the least common
# multiple of the simple sounds composing a consonance. With this designation
# the nature of the consonance itself is ascertained at once. In section 27,
# Chapter II, a method was described for finding the degree of agreeableness
# from the given exponent. The formula may be expressed by s-n+1, where s is
# the sum of the prime factors of the exponent and n is the number of these
# factors. The smaller the number yielded by this formula, the more agreeable
# or more easily perceived is the consonance under consideration."
#
# Vogel p. 148 adjust the -n+1 so "every 3,5,7 ... is taken fully in account".
#
# Euler p. 79 starts with least common multiple 1:2^n 1 is always in numerator
# for Euler the unision 1:1 = 1 + 1 - 2 + 1 = 1
# the perfect fifth 3:2 would be expressed as 1:6, n=2 3+2-2+1=4
# and the major 3rd 5:4 would be 1:20 2+2+5 = 9, n = 3 with -n+1 = 7
# for the major triad we would combine 3:2 with 5:4
# but to find common ground we need a 5 or a 4
# so we multiply 3:2 by 2 to give us 6:4 and then combine 6:4:5:4 -> 4:5:6
# so the major triad would be 1:60 n=4 2+2+3+5-4+1 = 9
#
# we start with ratios expressed as prime factor exponents = product(p^n_p)
# ratio = 1^n_1 * 2^n_2 * 3^n_3 * 5^n_5
# where n_p is positive for numerator and negative for denominator
# see examples here: https://en.xen.wiki/w/Monzo
#
# our dissonance measure is the sum of those sum(p*n_p) exponent prime factors
# dissonance = 1*n_1 + 2*n_2 + 3*n_3 + 5*n_5
# this ends up being almost the sum of the prime factors the only difference is
# that because any number raised to 0 is 1
# that means 1^0 = 1 for the ratio and so n_1 = 0 and so 1*n_1 = 0
#
# for us the perfect fifth is 3:2 = 5
# unison 1:1 = 0
# octave 2:1 = 2
# perfect fifth 3:2 = 5
# major 3rd 5:4 = 9
# and the major triad would 5:4:3:2 and we just sum up the exponent prime factors
#
#
# Euler:    1,11, 8, 8, 7, 5, 8.5, 4, 8, 7,  9, 10, 2
# Vogel:    1,12, 9, 9, 7, 5, 9.5, 4, 8, 8, 10, 11, 2
# Current:  0,16,12,10, 9, 7,  13, 5,11, 8, 14, 14, 2
#
# the results are similar but one interesting distinction is that
# the difference between the octave complements in the current measure
# is 2 which is the formula for the octave complement ri*r2=2
# whereas for Euler and Vogel the differences in octave complements is 1
dissonance <- function(.x) {
  checkmate::assert_integerish(.x)
  .x %>% sapply(exponent_prime_factors_sum) %>% sum
}

# we are using the semitone, the minor second m2 up, as the upper bound of dissonance
# we will subtract dissonance from this number to give us ascending consonance
#
# the result would be the same if we used major 7th down
#
# m2, is 1 in integer notation but R vectors are indexed from 1
# so that's why we have see + 1 notation
dissonance_upper_bound.uncached <- function() {
  minor_2nd = 1

  dissonance(c(ratios$up.numerator[minor_2nd+1],
               ratios$up.denominator[minor_2nd+1]))
}
dissonance_upper_bound <- memoise::memoise(dissonance_upper_bound.uncached)

exponent_prime_factors_sum <- function(.x) {
  checkmate::assert_integerish(.x)
  numbers::primeFactors(.x) %>% .[.>1] %>% sum
}
rotate <- function(.x,.y,.angle) {
  checkmate::assert_numeric(.angle)
  coordinates = rbind(.x,.y)
  R = tibble::frame_matrix(
    ~.x, ~.y,
    cos(.angle), -sin(.angle),
    sin(.angle), cos(.angle)
  )
  (R %*% coordinates * cos(.angle)) %>% zapsmall
}
