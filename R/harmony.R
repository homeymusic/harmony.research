harmony.uncached <- function(x, direction=+1, reference_tone=NULL, name=NULL) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(direction,c(-1,+1))
  checkmate::assert_integerish(reference_tone)

  t = tibble::tibble(
    position = x %>% mean,
    direction = direction,
    reference_tone = reference_tone,
    name = name,
    intervallic_name =intervallic_name(x,direction,reference_tone)
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

intervallic_name <- function(x, direction, reference_tone) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(direction,c(-1,+1))
  checkmate::assert_integerish(reference_tone)

  underlined_reference_tone = stringr::str_replace_all(reference_tone,"(.)","\\1\u0332")
  intervallic_name = x %>% sort %>% paste(collapse = ":")
  if (reference_tone %in% x) {
    intervallic_name=gsub(paste0("\\b",reference_tone,"\\b"),underlined_reference_tone,intervallic_name)
  } else {
    intervallic_name = paste(underlined_reference_tone,intervallic_name)
  }
  intervallic_name = paste(intervallic_name, ifelse (direction == 1, '\u21D1','\u21D3'), sep="")
  intervallic_name
}