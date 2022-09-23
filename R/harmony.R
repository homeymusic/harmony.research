#' Harmony
#'
#' Provides the musical harmony metrics of a note or chord.
#'
#'
#' @param chord A note or chord expressed as an interval integer or vector of interval integers
#' @param direction Harmonic direction +1 is up and -1 is down
#' @param root The reference tone of the chord or larger context
#' @param name A custom name for the note or chord
#' @return A tibble
#'
#' @export
harmony <- function(chord, direction=NULL, root=NULL, name=NULL) {
  checkmate::assert_integerish(chord)
  checkmate::assert_choice(direction,c(-1,0,+1),null.ok=TRUE)
  checkmate::assert_integerish(root,null.ok=TRUE)

  # TODO: assign chord positions to a variable for calculation of mean
  # and storage later.

  # build the harmony table
  t = tibble::tibble(
    # TODO: include integer_position and position in cents
    position                = chord %>% mean,
    name                    = name,
    explicit_direction      = direction,
    implicit_direction      = implicit_direction(chord,root),
    explicit_root           = root,
    implicit_root           = implicit_root(chord,direction),
    direction               = ifelse(is.null(direction),
                                     implicit_direction,
                                     direction),
    root                    = ifelse(is.null(root),
                                     implicit_root,
                                     root),
    integer_name            = integer_name(chord,direction,root)
  )
  # store the original chord
  attr(t,"chord") <- chord
  # TODO: store the chord positions as well?
  # store the aurally centered chord
  attr(t,"aurally_centered_chord") <- aurally_centered_chord(chord,t$direction,
                                                             t$root)
  # calculate tonic-octave dissonance
  tonic_octave_dissonance = tonic_octave_dissonance(attr(t,"aurally_centered_chord"))
  # flip orientation to tonic-octave consonance
  tonic_octave_consonance = max_dissonance() - tonic_octave_dissonance
  # rotate pi/4 (45 deg) to affinity-brightness
  affinity_brightness = tonic_octave_consonance %>% rotate(pi/4)
  # store the ABCs with L1 norm of affinity-brightness as consonance magnitude
  t %>% tibble::add_column(
    tonic_consonance  = tonic_octave_consonance[1,1],
    octave_consonance = tonic_octave_consonance[1,2],
    affinity          = affinity_brightness[1,2],
    brightness        = affinity_brightness[1,1],
    consonance        = abs(affinity_brightness[1,2]) + abs(affinity_brightness[1,1])
  )
}

#' @rdname harmony
#' @export
h <- harmony

tonic_octave_dissonance <- function(chord) {
  checkmate::assert_integerish(chord)
  # TODO: check for tritone 6 and return the average dissonance of the
  # two tritones 7:5, 10:7 symetrical around 600 cents

  cbind(
    sum_primes_chord(chord,ref.freq='tonic'),
    sum_primes_chord(chord,ref.freq='octave')
  )

}

sum_primes_chord <- function(chord,ref.freq) {
  checkmate::assert_integerish(chord)
  checkmate::assert_choice(ref.freq,c('tonic','octave'))

  direction = ifelse(ref.freq=='tonic',+1,-1)

  chord %>% purrr::map_dbl(~ frequency_ratio(.x,direction) %>% sum_primes_ratio) %>%
    mean
}

sum_primes_ratio <- function(ratio) {
  checkmate::assert_integerish(ratio)
  ratio %>% purrr::map_dbl(~ numbers::primeFactors(.x)[numbers::primeFactors(.x)>1] %>% sum) %>%
    sum
}

# we are using the semitone, the minor second m2 up, as the upper bound of dissonance
# we will subtract dissonance from this number to give us ascending consonance
#
# the result would be the same if we used major 7th M7 down
#
# m2, is 1 in integer notation but R vectors are indexed from 1
# so that's why we have see + 1 notation
max_dissonance <- function() {
  sum_primes_ratio(c(frequency_ratio(1,1)))
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

aurally_centered_chord <- function(chord,direction,root) {
  checkmate::assert_integerish(chord)
  checkmate::qassert(root,'X1')

  # move the chord to the aural center
  if (length(chord)==1) {
    chord
  } else if (direction >= 0) {
    chord - root
  } else {
    # adjust the aural root to the octave in case of inversion
    chord - root + 12
  }
}

implicit_root <- function(chord,explicit_direction) {
  if (!is.null(explicit_direction)) {
    if (length(chord)==1) {
      ifelse(explicit_direction<0,12,0)
    } else {
      ifelse(explicit_direction<0,max(chord),min(chord))
    }
  } else {
    if (length(chord)==1) {
      0
    } else {
      if (c(0,12) %in% chord %>% all) {
        0
      } else if (12 == min(chord) || 12 == max(chord)) {
        12
      } else if (0 == min(chord) || 0 == max(chord)) {
        0
      } else {
        min(chord)
      }
    }
  }
}

implicit_direction <- function(chord,explicit_root) {
  if (!is.null(explicit_root)) {
    # a root is given
    if (length(chord)==1) {
# ifelse(explicit_root==12,-1,1)
    } else {
      if (explicit_root<=min(chord)) {
        1
      } else if (explicit_root>=max(chord)) {
        -1
      } else {
        0
      }
    }
  } else {
    if (length(chord)==1) {
      0
    } else if (c(0,12) %in% chord %>% all) {
      0
    } else if (12 == max(chord) || 0 == max(chord)) {
      -1
    } else if (12 == min(chord) || 0 == min(chord)) {
      1
    } else if (12 %in% chord) {
      -1
    } else {
      1
    }
  }
}

integer_name <- function(chord, direction, root) {
  checkmate::assert_integerish(chord)
  checkmate::assert_choice(direction,c(-1,0,+1))
  checkmate::assert_integerish(root)

  up_arrow =   '\u21D1'  # ⇑
  down_arrow = '\u21D3'  # ⇓
  mixed_arrow = paste0(up_arrow,down_arrow) # ⇑⇓

  current_arrow = NULL
  if      (direction == -1) {arrow = down_arrow}
  else if (direction ==  0) {arrow = mixed_arrow}
  else if (direction == +1) {arrow = up_arrow}

  underlined_chord = underline(chord,root)
  if (direction==0) {
    underlined_chord = underline(underlined_chord,root+12)
  }
  underlined_chord %>% paste(collapse = ":") %>% paste0(arrow) %>%
    add_roots_without_chord(root,chord,direction)
}
underline <- function(chord,tone) {
  chord %>% sapply(function(x){
    if (x==tone) {
      stringr::str_replace_all(x,"(.)",paste0("\\1",'\u0332'))
    } else {
      x
    }
  })
}
add_roots_without_chord <- function(integer_name,root,chord,direction) {
  if (root %in% chord) {
    # do nothing
  } else {
    integer_name = paste(underline(root,root),integer_name)
    if (direction==0) {
      integer_name = paste(integer_name,underline(root+12,root+12))
    }
  }
  integer_name
}

# this is to stop R check command from complaining about magrittr
globalVariables(".")

# -------------------------------------------------------------------------



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
# we start with ratios() expressed as prime factor exponents = product(p^n_p)
# ratio = 1^n_1 * 2^n_2 * 3^n_3 * 5^n_5
# where n_p is positive for numerator and negative for denominator
# see examples here: https://enxen.wiki/w/Monzo
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

