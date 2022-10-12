pitch.uncached <- function(x) {
  checkmate::qassert(x,'X1')

  tonic.ratio  = frequency_ratio(x,observation_point=0)
  octave.ratio = frequency_ratio(x,observation_point=12)

  t <- tibble::tibble_row(
    integer         = x, # integer position
    # from the tonic observation point, the numerator of the frequency ratio
    # is the higher frequency for pitches above the tonic
    tonic.num.hi     = tonic.ratio[1],
    tonic.den.lo     = tonic.ratio[2],
    tonic.position   = 1200 * log2(.data$tonic.num.hi  / .data$tonic.den.lo), # cents
    # from the octave observation point, the numerator of the frequency ratio
    # is the lower frequency for pitches below the octave
    octave.num.lo    = octave.ratio[1],
    octave.den.hi    = octave.ratio[2],
    octave.position  = 1200 * log2(.data$octave.num.lo / .data$octave.den.hi), # cents
    # use the tonic position as the primary position in cents
    cents            = .data$tonic.position # position in cents
  )
}

#' Pitch
#'
#' Provides the metrics of a pitch
#'
#'
#' @param x A pitch expressed as an integer
#' @return A tibble
#'
#' @export
pitch <- memoise::memoise(pitch.uncached)

#' @rdname pitch
#' @export
p <- pitch

frequency_ratio <- function(x,observation_point) {
  checkmate::qassert(x,'X1')
  checkmate::assert_choice(observation_point,c(0,12))
  num <- den <- NULL

  if (observation_point == 0) {
    num = compound_ratio(x,'tonic.num.hi')
    den = compound_ratio(x,'tonic.den.lo')
  } else if (observation_point == 12) {
    num = compound_ratio(x,'octave.num.lo')
    den = compound_ratio(x,'octave.den.hi')
  }
  phonTools::reduce.fraction(c(num,den))
}

compound_ratio <- function(x,dimension) {
  checkmate::qassert(x,'X1')
  checkmate::assert_choice(dimension,c('tonic.num.hi','tonic.den.lo',
                                       'octave.num.lo','octave.den.hi'))

  pitch_class_ratios_for_dimension = pitch_class_ratios()[dimension] %>% unlist
  if (x>=0 && x<=12) {
    pitch_class_ratios_for_dimension[[x+1]]
  } else {
    integer = x %% 12
    pitch_class_ratio = pitch_class_ratios_for_dimension[[integer+1]]
    if ((x<0 && (dimension == 'tonic.den.lo' || dimension == 'octave.den.hi')) ||
        (x>12 && (dimension == 'tonic.num.hi' || dimension == 'octave.num.lo'))) {
      pitch_class_ratio * (2 ^ abs((x / 12) %>% floor))
    } else {
      pitch_class_ratio
    }
  }
}

pitch_class_ratios <- function() {

  pitch_ratios = 0:12 %>% sapply(pitch_ratio)

  tonic.num.hi = pitch_ratios[1,]
  tonic.den.lo = pitch_ratios[2,]

  list(
    #############################################
    # Tonic Frequency Ratios
    # pitch frequency: ascending
    tonic.num.hi = tonic.num.hi, # numerator
    # reference frequency: tonic
    tonic.den.lo   = tonic.den.lo, # denominator

    #############################################
    # Octave Frequency Ratios
    # pitch frequency: descending
    octave.num.lo = rev(tonic.den.lo), # numerator
    # reference frequency: octave
    octave.den.hi   = rev(tonic.num.hi)  # denominator
  )
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
