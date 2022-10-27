harmony.uncached <- function(chord, observation_point=NA, root=NA, name=NA,
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
  # calculate various consonance values
  #
  # mulloy2022      - sum of prime factors metric
  consonance.primes           = consonance.primes(centered_chord)
  colnames(consonance.primes) = paste0("primes.", colnames(consonance.primes))
  # akin to stolzenburg2015 - log periodicity metric
  consonance.stolzenburg2015  =
    consonance.stolzenburg2015(centered_chord)
  colnames(consonance.stolzenburg2015) = paste0("stolzenburg2015.",
                                                colnames(consonance.stolzenburg2015))
  # store all the consonance metrics
  t=tibble::add_column(t,consonance.primes,consonance.stolzenburg2015)
  # store the integer_name and the default consonance metric's affinity and brightness
  tibble::add_column(t,
                     integer_name = harmonic_integer_name(chord,t$observation_point,t$root),
                     label        = stringr::str_trim(paste(integer_name,na.omit(name),sep="\n")),
                     brightness   = t[[paste0(default_consonance_metric,'.brightness')]],
                     affinity     = t[[paste0(default_consonance_metric,'.affinity')]],
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

  up_arrow    = "\u2191" # "↑"
  down_arrow  = "\u2193" # "↓"
  mixed_arrow = paste0(up_arrow,down_arrow)

  current_arrow = NULL
  if      (is.na(observation_point)) {arrow = mixed_arrow}
  else if (observation_point == 12)  {arrow = down_arrow}
  else if (observation_point ==  0)  {arrow = up_arrow}

  underlined_chord = underline(chord,root)
  if (is.na(observation_point) && (root==0)) {
    underlined_chord = underline(underlined_chord,root+12)
  }
  underlined_chord %>% paste(collapse = ":") %>% paste0(arrow) %>%
    add_roots_outside_chord(root,chord,observation_point)
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
add_roots_outside_chord <- function(integer_name,root,chord,observation_point) {
  if (is.na(observation_point)) {
    if (c(root, root+12) %in% chord %>% all) {
      integer_name
    } else if (root %in% chord) {
      paste(integer_name,underline(root+12,root+12))
    } else if ((root + 12) %in% chord) {
      paste(underline(root,root),integer_name)
    } else {
      integer_name = paste(underline(root,root),integer_name)
      paste(integer_name,underline(root+12,root+12))
    }
  } else if (root %in% chord) {
    # do nothing
    integer_name
  } else {
    paste(underline(root,root),integer_name)
  }
}

coalesced_observation_point <- function(observation_point) {
  dplyr::coalesce(observation_point,0)
}
