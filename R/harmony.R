harmony.uncached <- function(chord, observation_point=NA, root=NA,
                             name=NA, midi_root = 60,
                             default_consonance_metric='stolzenburg2015') {
  checkmate::assert_integerish(chord)
  if (length(chord)==1) {
    checkmate::assert_choice(observation_point,NA)
  } else {
    checkmate::assert_choice(observation_point,c(0,NA,12))
  }
  checkmate::assert_integerish(root)
  checkmate::assert_character(name)
  checkmate::assert_integerish(midi_root,lower=0,upper=127)

  # build the harmony table
  t <- tibble::tibble_row(
    cents              = cents(chord),   # position in cents
    integer            = chord %>% mean, # integer position
    name               = name,
    midi_root          = midi_root,
    explicit_root      = root,
    explicit_observation_point = observation_point,
    guessed_root       = guessed_root(chord,.data$explicit_observation_point),
    root               = ifelse(is.na(.data$explicit_root),
                                guessed_root,
                                .data$explicit_root),
    guessed_observation_point = guessed_observation_point(chord,.data$explicit_root,guessed_root),
    observation_point  = ifelse(is.na(.data$explicit_observation_point),
                                guessed_observation_point,
                                .data$explicit_observation_point)
  )
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
                     label          = stringr::str_trim(
                       paste(.data$classical_name,.data$integer_name,stats::na.omit(name),sep="\n")),
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

  underline(chord,observation_point,root) %>%
    paste(collapse = ":") %>%
    paste0(arrow(observation_point)) %>%
    add_roots_outside_chord(root,chord,observation_point)
}
harmonic_classical_name <- function(chord, observation_point, root, midi_root) {
  checkmate::assert_integerish(chord)
  checkmate::assert_choice(observation_point,c(0,NA,12))
  checkmate::assert_integerish(root)
  checkmate::assert_integerish(midi_root,lower=0,upper=127)

  midi_chord = chord - root + midi_root
  underline(midi_chord,observation_point,midi_root,classical=TRUE) %>%
    paste(collapse = ":") %>%
    paste0(arrow(observation_point)) %>%
    add_roots_outside_chord(root,chord,observation_point,midi_root=midi_root,classical=TRUE)
}
underline <- function(chord,observation_point,root,classical=FALSE) {
  checkmate::assert_integerish(chord)
  checkmate::assert_choice(observation_point,c(0,NA,12))
  checkmate::assert_integerish(root)

  chord %>% sapply(function(x){
    pitch = if (classical) {classical_pitch_label(x)} else {x}
    if ((x==root)|| (is.na(observation_point) && (root==0) && (x==(root+12)))) {
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
  root_pitch = if (classical) {classical_pitch_label(midi_root - root)} else {root}
  upper_root_pitch = if (classical) {classical_pitch_label(midi_root-root+12)} else {root+12}
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
classical_pitch_label <- function(x) {
  checkmate::assert_integerish(x,lower=0,upper=127)
  pitch_class_flats = c('C', 'D\U266D', 'D', 'E\U266D', 'E', 'F',
                        'G\U266D', 'G', 'A\U266D', 'A', 'B\U266D', 'B')
  pitch_class_sharps = c('C', 'C\U266F', 'D', 'D\U266F', 'E', 'F',
                        'F\U266F', 'G', 'G\U266F', 'A', 'A\U266F', 'B')
  octave = (x / 12) %>% trunc - 1
  pitch_class_name = c(paste0(pitch_class_flats[x %% 12 +1],octave),
                       paste0(pitch_class_sharps[x %% 12 +1],octave)) %>% unique
  paste(pitch_class_name,collapse='|')
}
coalesced_observation_point <- function(observation_point) {
  dplyr::coalesce(observation_point,0)
}
