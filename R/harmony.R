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

  # stub out the harmony table
  t = tibble::tibble(
    position                = chord %>% mean,
    name                    = name
  )
  # store the original chord
  attr(t,"chord") <- chord

  a = chord_analysis(chord,direction,root)

  # calculate the ABCs affinity, brightness and consonance
  # flip from tonic-octave dissonance to tonic-octave consonance
  # rotate coordinate system to brightness-affinity
  affinity_brightness = (max_dissonance() -
                           tonic_octave_dissonance(
                             attr(a,"aurally_centered_chord"))) %>% rotate(pi/4)
  # store the ABCs including L1 norm of affinity-brightness as consonance magnitude
  a$affinity         = affinity_brightness[1,2]
  a$brightness       = affinity_brightness[1,1]
  a$consonance       = abs(a$brightness) + abs(a$affinity)

  # create the integer name that shows root (underlines) and inversion (arrow)
  a$integer_name = integer_name(chord,a$direction,a$root)

  # store the aurally centered chord on the tibble
  attr(t,"aurally_centered_chord") <- attr(a,"aurally_centered_chord")
  # add the harmonic parameters to the tibble
  dplyr::bind_cols(t,a)
}
#' @rdname harmony
#' @export
h <- harmony

tonic_octave_dissonance <- function(chord) {
  checkmate::assert_integerish(chord)
  # TODO: check for tritone 6 and return the average dissonance of the
  # two tritones 7:5, 10:7 symetrical around 600 cents

  cbind(
    chord %>% purrr::map(function(tone) {
      # tonic ascending ratios
      sum_primes(frequency_ratio(tone,1))}) %>% unlist %>% mean,

    chord %>% purrr::map(function(tone) {
      # octave descending ratios
      sum_primes(frequency_ratio(tone,-1))}) %>% unlist %>% mean
  )

}

sum_primes <- function(ratios) {
  checkmate::assert_integerish(ratios)

  ratios %>% purrr::map(numbers::primeFactors) %>% unlist %>% sum
}

# we are using the semitone, the minor second m2 up, as the upper bound of dissonance
# we will subtract dissonance from this number to give us ascending consonance
#
# the result would be the same if we used major 7th M7 down
#
# m2, is 1 in integer notation but R vectors are indexed from 1
# so that's why we have see + 1 notation
max_dissonance <- function() {
  sum_primes(c(frequency_ratio(1,1)))
}

rotate <- function(.coordinates,.angle) {
  checkmate::assert_numeric(.angle)
  .coordinates = t(.coordinates)
  R = tibble::frame_matrix(
    ~chord,          ~.y,
    cos(.angle), -sin(.angle),
    sin(.angle),  cos(.angle)
  )
  (R %*% .coordinates * cos(.angle)) %>% zapsmall %>% t
}

chord_analysis <- function(chord,explicit_direction,explicit_root) {
  checkmate::assert_integerish(chord)
  checkmate::assert_choice(explicit_direction,c(-1,0,+1),null.ok=TRUE)
  checkmate::assert_integerish(explicit_root,null.ok=TRUE)

  a = list(explicit_direction = explicit_direction,
           implicit_direction = implicit_direction(chord,explicit_root),
           explicit_root      = explicit_root,
           implicit_root      = implicit_root(chord,explicit_direction))

  a$direction                 = ifelse(is.null(a$explicit_direction),
                                       a$implicit_direction,
                                       a$explicit_direction)
  a$root                      = ifelse(is.null(a$explicit_root),
                                       a$implicit_root,
                                       a$explicit_root)

  # move the chord to the aural center
  if(length(chord)>1) {
    aurally_centered_chord = chord - a$root
    # adjust the aural root to the octave in case of inversion
    if (a$direction < 0) {aurally_centered_chord = aurally_centered_chord + 12}
  } else {
    aurally_centered_chord = chord
  }
  # store centered chord on the params object
  attr(a,"aurally_centered_chord") <- aurally_centered_chord

  a
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

