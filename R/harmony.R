#' Harmony
#'
#' Provides the harmonic metrics of a note or chord.
#'
#'
#' @param chord A pitch or chord expressed as an interval integer or vector of interval integers
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
    position           = position(chord), # cents
    integer_position   = chord %>% mean,
    name               = name,
    explicit_direction = direction,
    guessed_direction  = guessed_direction(chord,root),
    explicit_root      = root,
    guessed_root       = guessed_root(chord,direction),
    direction          = ifelse(is.null(direction),
                                guessed_direction,
                                direction),
    root               = ifelse(is.null(root),
                                guessed_root,
                                root),
    integer_name       = integer_name(chord,direction,root)
  )
  # store the original chord
  attr(t,"chord") <- chord
  # store the aurally centered chord
  attr(t,"centered_chord") <- centered_chord <-
    centered_chord(chord, t$direction, t$root)

  t %>% tibble::add_column(
    affinity          = affinity(centered_chord),
    brightness        = brightness(centered_chord),
  )
}

#' @rdname harmony
#' @export
h <- harmony

centered_chord <- function(chord,direction,root) {
  checkmate::assert_integerish(chord)
  checkmate::qassert(root,'X1')

  if (direction >= 0) {
    chord - root
  } else {
    chord - root + 12
  }
}

affinity <- function(chord) {
  checkmate::assert_integerish(chord)
  chord %>% purrr::map_dbl(~pitch(.x)$affinity) %>% mean
}

brightness <- function(chord) {
  checkmate::assert_integerish(chord)
  chord %>% purrr::map_dbl(~pitch(.x)$brightness) %>% mean
}

position <- function(chord) {
  checkmate::assert_integerish(chord)
  chord %>% purrr::map_dbl(~pitch(.x)$tonic.position) %>% mean
}

guessed_root <- function(chord,explicit_direction) {
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
        print('12')
        12
      } else if (0 == min(chord) || 0 == max(chord)) {
        0
      } else {
        min(chord)
      }
    }
  }
}

guessed_direction <- function(chord,explicit_root) {
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
    add_roots_outside_chord(root,chord,direction)
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
add_roots_outside_chord <- function(integer_name,root,chord,direction) {
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

