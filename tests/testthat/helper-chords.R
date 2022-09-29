major_triads <- function() {
  list(
    "root"=h(c(0,4,7),
             root=0,
             direction=+1,
             name="major triad"),
    "1st inversion"=h(c(0+12,4,7),
                      root=12,
                      direction=-1,
                      name="major triad 1st inversion"),
    "2nd inversion"=h(c(0+12,4+12,7),
                      root=12,
                      direction=-1,
                      name="major triad 2nd inversion")
  )
}
minor_triads <- function() {
  list(
    "root"=h(c(0,3,7),
             root=0,
             direction=+1,
             name="minor triad"),
    "1st inversion"=h(c(0+12,3,7),
                      root=12,
                      direction=-1,
                      name="minor triad 1st inversion"),
    "2nd inversion"=h(c(0+12,3+12,7),
                      root=12,
                      direction=-1,
                      name="minor triad 2nd inversion")
  )
}
intervals <- function() {
  tibble::tibble(
    integer_position = 0:12,
    name = c("tonic","minor 2nd","major 2nd","minor 3rd","major 3rd",
             "perfect 4th","tritone","perfect 5th","minor 6th",
             "major 6th","minor 7th","major 7th","octave"),
    brightness =  c(1,-1,1,-1,1,-1,0,1,-1,1,-1,1,-1),
    affinity =    c(15,1,3,7,6,10,4,10,6,7,3,1,15)
  )
}
diatonic_scales <- function() {
  list(
    'locrian'=h(c(0,1,3,5,6,8,10,12), name = 'locrian'),
    'phrygian'=h(c(0,1,3,5,7,8,10,12), name = 'phrygian'),
    'aeolian'=h(c(0,2,3,5,7,8,10,12), name = 'aeolian'),
    'dorian'=h(c(0,2,3,5,7,9,10,12), name = 'dorian'),
    'mixolydian'=h(c(0,2,4,5,7,9,10,12), name = 'mixolydian'),
    'ionian'=h(c(0,2,4,5,7,9,11,12), name = 'ionian'),
    'lydian'=h(c(0,2,4,6,7,9,11,12), name = 'lydian')
  )
}
major_triad_progression <- function(){
  major_triad = c(0,4,7)
  list(
    h(major_triad  , direction = +1),
    h(major_triad+5, direction = +1),
    h(major_triad+7, direction = +1),
    h(major_triad  , direction = +1)
  )
}
voice_leading_progression <- function(){
  major_triad = c(0,4,7)
  list(
    h(major_triad  , direction = +1),
    h(major_triad+1, direction = +1),
    h(major_triad+2, direction = +1),
    h(major_triad+3, direction = +1),
    h(major_triad+4, direction = +1),
    h(major_triad+5, direction = +1),
    h(major_triad+6, direction = +1),
    h(major_triad+7, direction = +1),
    h(major_triad+8, direction = +1),
    h(major_triad+9, direction = +1),
    h(major_triad+10, direction = +1),
    h(major_triad+11, direction = +1),
    h(major_triad+12, direction = +1),
    h(major_triad+13, direction = +1),
    h(major_triad  , direction = +1)
  )
}

