rotate <- function(coordinates,angle) {
  checkmate::assert_numeric(angle)
  coordinates = t(coordinates)
  R = tibble::frame_matrix(
    ~chord,          ~.y,
    cos(angle), -sin(angle),
    sin(angle),  cos(angle)
  )
  (R %*% coordinates * cos(angle)) %>% zapsmall %>% t
}
