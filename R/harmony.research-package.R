#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom checkmate assert_integerish assert_choice qassert assert_character
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom purrr pmap prepend
#' @importFrom numbers primeFactors
#' @importFrom rlang .data
#' @importFrom dplyr bind_rows bind_cols
#' @importFrom tidyr expand_grid
#' @importFrom utils head
#' @importFrom memoise memoise
#' @importFrom phonTools reduce.fraction
#' @importFrom gmp lcm.default
## usethis namespace: end
NULL

TONIC  <- 0
OCTAVE <- 12

# this is to stop R check command from complaining about magrittr
globalVariables(".")
