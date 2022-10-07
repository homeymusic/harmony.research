pitch.uncached <- function(x) {
  checkmate::qassert(x,'X1')

  t <- tibble::tibble_row(
    integer_position = x,
    tonic.pitch      = compound_ratios(x,'tonic.pitch'),      # numerator
    tonic.ref        = compound_ratios(x,'tonic.ref'),       # denominator
    tonic.position   = 1200 * log2(.data$tonic.pitch  / .data$tonic.ref), # cents
    position         = .data$tonic.position,
    octave.pitch     = compound_ratios(x,'octave.pitch'),     # numerator
    octave.ref       = compound_ratios(x,'octave.ref'),      # denominator
    octave.position  = 1200 * log2(.data$octave.pitch / .data$octave.ref) # cents
  )
}

#' Pitch
#'
#' Provides the metrics of a pitch
#'
#'
#' @param x A pitch expressed as an interval integer
#' @return A tibble
#'
#' @export
pitch <- memoise::memoise(pitch.uncached)

#' @rdname pitch
#' @export
p <- pitch

pitch_class_ratios <- function() {

  pitch_ratios = 0:12 %>% sapply(pitch_ratio)

  tonic.pitch  = pitch_ratios[1,]
  tonic.ref    = pitch_ratios[2,]

  list(
    #############################################
    # Tonic Frequency Ratios
    # pitch frequency: ascending
    tonic.pitch = tonic.pitch, # numerator
    # reference frequency: tonic
    tonic.ref   = tonic.ref, # denominator

    #############################################
    # Octave Frequency Ratios
    # pitch frequency: descending
    octave.pitch = rev(tonic.ref), # numerator
    # reference frequency: octave
    octave.ref   = rev(tonic.pitch)  # denominator
  )
}

compound_ratios <- function(x,dimension) {
  checkmate::qassert(x,'X1')
  checkmate::assert_choice(dimension,c('tonic.pitch','tonic.ref',
                                       'octave.pitch','octave.ref'))

  if (x>=0 && x<=12) {
    # ratios are in the primary pitch class octave so all done
    (pitch_class_ratios()[dimension] %>% unlist)[[x+1]]
  } else {
    # else ratios are above or below the primary octave
    # start with the pitch class ratio
    integer = x %% 12
    pitch_class_ratio = (pitch_class_ratios()[dimension] %>% unlist)[[integer+1]]
    # calculate the octave adjustment
    octave_multiplier = 2 ^ abs((x / 12) %>% floor)

    if (x>12 && (dimension == 'tonic.pitch' || dimension == 'octave.pitch')) {
      # above the primary octave and the current dimension is the numerator
      # apply octave adjustment to the numerator
      pitch_class_ratio * octave_multiplier
    } else if (x<0 && (dimension == 'tonic.ref' || dimension == 'octave.ref')) {
      # below the primary octave and the current dimension is the denominator
      # apply octave adjustment to the denominator
      pitch_class_ratio * octave_multiplier
    } else {
      # the current dimension does not need to change
      pitch_class_ratio
    }
  }
}

# from https://github.com/pmcharrison/stolz15
# See DOI: 10.1080/17459737.2015.1033024
# @param x Number to approximate
# @param d Tolerance ratio
pitch_ratio <- function(x, d=0.0102) {
  checkmate::qassert(x,'X1')

  x = 2 ^ (x / 12)

  x_min <- (1 - d) * x
  x_max <- (1 + d) * x
  a_l <- floor(x)
  b_l <- 1
  a_r <- floor(x) + 1
  b_r <- 1
  a <- round(x)
  b <- 1
  while(a / b < x_min || x_max < a / b) {
    x_0 <- 2 * x - a / b
    if (x < a / b) {
      a_r <- a
      b_r <- b
      k <- floor((x_0 * b_l - a_l) / (a_r - x_0 * b_r))
      a_l <- a_l + k * a_r
      b_l <- b_l + k * b_r
    } else {
      a_l <- a
      b_l <- b
      k <- floor((a_r - x_0 * b_r) / (x_0 * b_l - a_l))
      a_r <- a_r + k * a_l
      b_r <- b_r + k * b_l
    }
    a <- a_l + a_r
    b <- b_l + b_r
  }
  c(a, b)
}
