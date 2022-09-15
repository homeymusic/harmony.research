major_triads <- function() {
  list(
    "root"=h(c(0,4,7),
             direction=+1,
             reference_tone=0,
             name="major triad"),
    "1st inversion"=h(c(0+12,4,7),
                      direction=-1,
                      reference_tone=12,
                      name="major triad 1st inversion"),
    "2nd inversion"=h(c(0+12,4+12,7),
                      direction=-1,
                      reference_tone=12,
                      name="major triad 2nd inversion")
  )
}
