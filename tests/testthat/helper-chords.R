major_triads <- function() {
  list(
    "root"=h(c(0,4,7),
             .reference_tone=0,
             .name="major triad"),
    "1st inversion"=h(c(0+12,4,7),
                      .reference_tone=12,
                      .name="major triad 1st inversion"),
    "2nd inversion"=h(c(0+12,4+12,7),
                      .reference_tone=12,
                      .name="major triad 2nd inversion")
  )
}
intervals <- function() {
  tibble::tibble(
    position = 0:12,
    name = c("tonic","minor 2nd","major 2nd","minor 3rd","major 3rd",
             "perfect 4th","tritone","perfect 5th","minor 6th",
             "major 6th","minor 7th","major 7th","octave"),
    brightness =  c(1,-1,1,-1,1,-1,0,1,-1,1,-1,1,-1),
    affinity =    c(15,1,3,7,6,10,4,10,6,7,3,1,15)
  )
}
