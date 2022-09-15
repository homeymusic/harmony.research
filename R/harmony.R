harmony.uncached <- function(x, direction=+1, reference_tone=NULL, name=NULL) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(direction,c(-1,+1))
}

#' Harmony
#'
#' Provides the musical harmony metrics of a note or chord.
#'
#' harmony(c(0,4,7)) # C Major
#' harmony(c(0,4,7),direction=1,name="C Major")
#' harmony(c(0+12,4,7),direction=-1,name="C Major 1st Inversion")
#' harmony(c(0+12,4+12,7),direction=1,name="C Major 2nd Inversion")
#'
#' @param x A note or chord expressed as an interval integer or vector of interval integers
#' @param direction Harmonic direction +1 is up and -1 is down
#' @param reference_tone The reference tone of the chord or larger context
#' @param name A custom name for the note or chord
#' @return A tibble with interval, intervallic_name, name, affinity, brightness and magnitude
#'
#' @export
harmony <- memoise::memoise(harmony.uncached)

#' @rdname harmony
#' @export
h <- harmony
