harmony.uncached <- function(chord, observation_point=NA, root=NA,
                             name=NA, midi_root = NA,
                             default_consonance_metric='stolzenburg2015') {
  checkmate::assert_integerish(chord)
  if (length(chord)==1) {
    checkmate::assert_choice(observation_point,NA)
  } else {
    checkmate::assert_choice(observation_point,c(0,NA,12))
  }
  checkmate::assert_integerish(root)
  checkmate::assert_character(name)

  # build the harmony table
  t <- tibble::tibble_row(
    cents              = cents(chord),   # position in cents
    integer            = chord %>% mean, # integer position
    name               = name,
    explicit_root      = root,
    explicit_observation_point = observation_point,
    explicit_midi_root = midi_root,
    guessed_root       = guessed_root(chord,.data$explicit_observation_point),
    root               = ifelse(is.na(.data$explicit_root),
                                guessed_root,
                                .data$explicit_root),
    guessed_observation_point = guessed_observation_point(chord,.data$explicit_root,guessed_root),
    observation_point  = ifelse(is.na(.data$explicit_observation_point),
                                guessed_observation_point,
                                .data$explicit_observation_point),
    guessed_midi_root  = guessed_midi_root(.data$root,.data$explicit_midi_root),
    midi_root          = ifelse(is.na(.data$explicit_midi_root),
                                guessed_midi_root,
                                .data$explicit_midi_root),
  )
  # check midi root is valid
  checkmate::assert_integerish(t$midi_root,lower=0,upper=127)
  # store the original chord
  attr(t,"chord") <- chord
  # store the aurally centered chord
  attr(t,"centered_chord") <- centered_chord <-
    centered_chord(chord, t$observation_point, t$root)

  ##########################################
  # calculate various consonance metrics
  #
  # mulloy2022      - primes with boundary condition
  consonance.primes           = consonance.primes(centered_chord)
  colnames(consonance.primes) = paste0("primes.",
                                       colnames(consonance.primes))
  # stolzenburg2015 - periodicity (without smoothing)
  consonance.stolzenburg2015  =
    consonance.stolzenburg2015(centered_chord)
  colnames(consonance.stolzenburg2015) = paste0("stolzenburg2015.",
                                                colnames(consonance.stolzenburg2015))
  # hutchinson1978 - roughness
  consonance.hutchinson1978  =
    consonance.hutchinson1978(centered_chord)
  colnames(consonance.hutchinson1978) = paste0("hutchinson1978.",
                                               colnames(consonance.hutchinson1978))

  # store all the consonance metrics
  t=tibble::add_column(t,consonance.primes,consonance.stolzenburg2015,consonance.hutchinson1978)
  # store the integer_name and the default consonance metric's affinity and brightness
  tibble::add_column(t,
                     integer_name   = harmonic_integer_name(
                       chord,t$observation_point,t$root),
                     classical_name = harmonic_classical_name(
                       chord,t$observation_point,t$root,t$midi_root),
                     label          = stringr::str_trim(paste(sep="\n",
                       .data$classical_name,.data$integer_name,stats::na.omit(name))),
                     brightness     = t[[paste0(
                       default_consonance_metric,'.brightness')]],
                     affinity       = t[[paste0(
                       default_consonance_metric,'.affinity')]],
                     .after='name')
}

#' Harmony
#'
#' Provides the harmonic metrics of a note or chord.
#'
#'
#' @param chord A pitch or chord expressed as an interval integer or vector of interval integers
#' @param observation_point Harmonic observation_point 0 is tonic, 12 is octave, NA is symmetrical
#' @param root The reference pitch of the chord or larger context
#' @param midi_root The MIDI root of the chord, defaults to 60 for using 0-based integer notation
#' @param name A custom name for the note or chord
#' @param default_consonance_metric The metric that will populate affinity and brightness values
#' @return A tibble
#'
#' @export
harmony <- memoise::memoise(harmony.uncached)

#' @rdname harmony
#' @export
h <- harmony

centered_chord <- function(chord,observation_point,root) {
  checkmate::assert_integerish(chord)
  checkmate::qassert(root,'X1')

  chord - root + coalesced_observation_point(observation_point)
}

cents <- function(chord) {
  checkmate::assert_integerish(chord)
  chord %>% purrr::map_dbl(~pitch(.x)$cents) %>% mean
}

guessed_root <- function(chord,explicit_observation_point) {
  if (!is.na(explicit_observation_point)) {
    if (length(chord)==1) {
      explicit_observation_point
    } else {
      ifelse(explicit_observation_point==12,max(chord),min(chord))
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

guessed_observation_point <- function(chord,explicit_root,guessed_root) {
  if (length(chord)==1) {
    NA
  } else if (!is.na(explicit_root)) {
    if (explicit_root<=min(chord)) {
      0
    } else if (explicit_root>=max(chord)) {
      12
    } else {
      NA
    }
  } else {
    if (c(guessed_root,guessed_root+12) %in% chord %>% all) {
      NA
    } else if (12 == max(chord) || 0 == max(chord)) {
      12
    } else if (12 == min(chord) || 0 == min(chord)) {
      0
    } else if (12 %in% chord) {
      12
    } else {
      0
    }
  }
}
harmonic_integer_name <- function(chord, observation_point, root) {
  checkmate::assert_integerish(chord)
  checkmate::assert_choice(observation_point,c(0,NA,12))
  checkmate::assert_integerish(root)

  integer_notation = underline(chord,observation_point,root) %>%
    paste(collapse = ":")

  paste0('{',integer_notation,'}',arrow(observation_point)) %>%
    add_roots_outside_chord(root,chord,observation_point)
}
harmonic_classical_name <- function(chord, observation_point, root, midi_root) {
  checkmate::assert_integerish(chord)
  checkmate::assert_choice(observation_point,c(0,NA,12))
  checkmate::assert_integerish(root)
  checkmate::assert_integerish(midi_root,lower=0,upper=127)

  underline(chord,observation_point,root,classical=TRUE,midi_root=midi_root) %>%
    paste(collapse = ":") %>%
    paste0(arrow(observation_point)) %>%
    add_roots_outside_chord(root,chord,observation_point,midi_root=midi_root,classical=TRUE)
}
underline <- function(chord,observation_point,root,classical=FALSE,midi_root=NA) {
  checkmate::assert_integerish(chord)
  checkmate::assert_choice(observation_point,c(0,NA,12))
  checkmate::assert_integerish(root)
  checkmate::assert_integerish(midi_root,lower=0,upper=127)

  chord %>% sapply(function(x){
    pitch = if (classical) {classical_pitch_label(x+midi_root,observation_point)} else {x}
    if ((x==root) || (is.na(observation_point) && (root==0) && (x==(root+12)))) {
      underline_pitch(pitch)
    } else {
      pitch
    }
  })
}
underline_pitch <- function(x) {
  stringr::str_replace_all(x,"(.)",paste0("\\1",'\u0332'))
}
arrow <- function(observation_point) {
  up_arrow    = "\u2191" # "↑"
  down_arrow  = "\u2193" # "↓"
  mixed_arrow = paste0(up_arrow,down_arrow)

  if      (is.na(observation_point)) {mixed_arrow}
  else if (observation_point == 12)  {down_arrow}
  else if (observation_point ==  0)  {up_arrow}
}
add_roots_outside_chord <- function(integer_name,root,chord,observation_point,midi_root=NA,classical=FALSE) {
  if (classical) {
    checkmate::assert_integerish(midi_root,lower=0,upper=127)
  }
  root_pitch = if (classical) {classical_pitch_label(midi_root,observation_point)} else {root}
  upper_root_pitch = if (classical) {classical_pitch_label(midi_root+12,observation_point)} else {root+12}
  if (is.na(observation_point)) {
    if (c(root, root+12) %in% chord %>% all) {
      integer_name
    } else if (root %in% chord) {
      paste(integer_name,underline_pitch(upper_root_pitch))
    } else if ((root + 12) %in% chord) {
      paste(underline_pitch(root_pitch),integer_name)
    } else {
      integer_name = paste(underline_pitch(root_pitch),integer_name)
      paste(integer_name,underline_pitch(upper_root_pitch))
    }
  } else if (root %in% chord) {
    # do nothing
    integer_name
  } else {
    paste(underline_pitch(root_pitch),integer_name)
  }
}
classical_pitch_label <- function(x, observation_point) {
  checkmate::assert_integerish(x,lower=0,upper=127)
  pitch_class_flats = c('C', 'Db', 'D', 'Eb', 'E', 'F',
                        'Gb', 'G', 'Ab', 'A', 'Bb', 'B')
  pitch_class_sharps = c('C', 'C#', 'D', 'D#', 'E', 'F',
                        'F#', 'G', 'G#', 'A', 'A#', 'B')
  octave = (x / 12) %>% trunc - 1
  pitch_class = if (is.na(observation_point) || observation_point == 0) {
    pitch_class_flats
  } else {
    pitch_class_sharps
  }
  paste0(pitch_class[x %% 12 +1],octave)
}
coalesced_observation_point <- function(observation_point) {
  dplyr::coalesce(observation_point,0)
}
guessed_midi_root <- function(root,explicit_midi_root) {
  if (is.na(explicit_midi_root)) {
    60
  } else {
    explicit_midi_root
  }
}
