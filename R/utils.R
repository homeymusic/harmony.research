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
chord_combinations <- function(combos) {
  checkmate::assert_list(combos)
  chords = purrr::map(combos,function(combo){
    params = expand.grid(observation_point=c(TONIC,OCTAVE),root=combo)
    purrr::map2(params$observation_point,params$root,
                ~h(combo,observation_point=.x,root=.y))
  })
  dplyr::bind_rows(chords)
}
