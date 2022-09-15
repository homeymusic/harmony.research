harmony.uncached <- function(x, direction=+1, reference_tone=NULL, name=NULL) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(direction,c(-1,+1))

  t = tibble::tibble(
    position = x %>% mean,
    direction = direction,
    reference_tone = reference_tone,
    name = name
  )
  attr(t,"chord") <- x
  t
}

#' Harmony
#'
#' Provides the musical harmony metrics of a note or chord.
#'
#'
#' @param x A note or chord expressed as an interval integer or vector of interval integers
#' @param direction Harmonic direction +1 is up and -1 is down
#' @param reference_tone The reference tone of the chord or larger context
#' @param name A custom name for the note or chord
#' @return A tibble
#'
#' @export
harmony <- memoise::memoise(harmony.uncached)

#' @rdname harmony
#' @export
h <- harmony
