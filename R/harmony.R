#' Harmony
#'
#' Provides the harmonic metrics of a note or chord.
#'
#'
#' @param chord A note or chord expressed as an interval integer or vector of interval integers
#' @param direction Harmonic direction +1 is up and -1 is down
#' @param root The reference pitch of the chord or larger context
#' @param name A custom name for the note or chord
#' @return A tibble
#'
#' @export
harmony <- function(chord, direction=NULL, root=NULL, name=NULL) {
  checkmate::assert_integerish(chord)
  if (length(chord)==1) {
    checkmate::assert_choice(direction,0,null.ok=TRUE)
  } else {
    checkmate::assert_choice(direction,c(-1,0,+1),null.ok=TRUE)
  }
  checkmate::assert_integerish(root,null.ok=TRUE)
  checkmate::assert_character(name,null.ok=TRUE)


  # build the harmony table
  t <- tibble::tibble_row(
    position                = position(chord), # cents
    integer_position        = chord %>% mean,
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
  # store the aurally centered chord
  attr(t,"aurally_centered_chord") <- aurally_centered_chord(chord,t$direction,
                                                             t$root)

  ###################################################################################
  # this is the heavy lifting for calculating affinity, brightness and consonance
  #
  # using the centered chord, calculate 2-dimensional tonic-octave dissonance
  tonic_octave_dissonance = tonic_octave_dissonance(attr(t,"aurally_centered_chord"))
  # flip orientation to 2-dimensional tonic-octave consonance
  tonic_octave_consonance = max_dissonance() - tonic_octave_dissonance
  # rotate pi/4 (45 deg) to 2-dimensional affinity-brightness
  affinity_brightness = tonic_octave_consonance %>% rotate(pi/4)

  # store the ABCs with L1 norm of affinity-brightness as consonance magnitude
  t %>% tibble::add_column(
    tonic.consonance  = tonic_octave_consonance[1,1],
    octave.consonance = tonic_octave_consonance[1,2],
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

  cbind(
    chord_primes(chord,'tonic.primes'),
    chord_primes(chord,'octave.primes')
  )

}

chord_primes <- function(chord,tonic_or_octave) {
  checkmate::assert_integerish(chord)
  checkmate::assert_choice(tonic_or_octave,c('tonic.primes','octave.primes'))

  chord %>% purrr::map(~pitch(.x)[tonic_or_octave]) %>% unlist %>% mean
}

# we are using the semitone, the minor second m2 up, as the upper bound of dissonance
# we will subtract dissonance from this number to give us ascending consonance
#
# the result would be the same if we used major 7th M7 down
#
# m2, is 1 in integer notation but R vectors are indexed from 1
# so that's why we have see + 1 notation
max_dissonance <- function() {
  pitch(1)$tonic.primes
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

  if (direction >= 0) {
    chord - root
  } else {
    # adjust the aural root to the octave in case of inversion
    chord - root + 12
  }
}

position <- function(chord) {
  checkmate::assert_integerish(chord)
  chord %>% purrr::map(~pitch(.x)['tonic.position']) %>% unlist %>% mean
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
  if (length(chord)==1) {
    0
  } else if (!is.null(explicit_root)) {
    if (explicit_root<=min(chord)) {
      1
    } else if (explicit_root>=max(chord)) {
      -1
    } else {
      0
    }
  } else {
    if (c(0,12) %in% chord %>% all) {
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
underline <- function(chord,pitch) {
  chord %>% sapply(function(x){
    if (x==pitch) {
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

