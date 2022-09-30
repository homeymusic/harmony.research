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

#################
# tonic chords
#
lydian_tonic_chords <- function() {
  list("I"=c(0,4,7),
       "II"=c(2,6,9),
       "iii"=c(4,7,11),
       "iv\u00B0"=c(6,9,12),
       "V"=c(7,11,14),
       "vi"=c(9,12,16),
       "vii"=c(11,14,18)
  )
}
ionian_tonic_chords <- function() {
  list("I"         =h(c( 0, 4, 7),direction=+1,name='I'),
       "ii"        =h(c( 2, 5, 9),direction=+1,name='ii'),
       "iii"       =h(c( 4, 7,11),direction=+1,name='iii'),
       "IV"        =h(c( 5, 9,12),direction=+1,name='IV'),
       "V"         =h(c( 7,11,14),direction=+1,name='V'),
       "vi"        =h(c( 9,12,16),direction=+1,name='vi'),
       "vii\u00B0" =h(c(11,14,17),direction=+1,name='vii\u00B0')
  )
}
mixolydian_tonic_chords <- function() {
  list("I"=c(0,4,7),
       "ii"=c(2,5,9),
       "iii\u00B0"=c(4,7,10),
       "IV"=c(5,9,12),
       "v"=c(7,10,14),
       "vi"=c(9,12,16),
       "VII"=c(10,14,17)
  )
}
dorian_tonic_chords <- function() {
  list("i"=c(0,3,7),
       "ii"=c(2,5,9),
       "III"=c(3,7,10),
       "IV"=c(5,9,12),
       "v"=c(7,10,14),
       "vi\u00B0"=c(9,12,15),
       "VII"=c(10,14,17)
  )
}
aeolian_tonic_chords <- function() {
  list("i"=c(0,3,7),
       "ii\u00B0"=c(2,5,8),
       "III"=c(3,7,10),
       "iv"=c(5,8,12),
       "v"=c(7,10,14),
       "VI"=c(8,12,15),
       "VII"=c(10,14,17)
  )
}
phrygian_tonic_chords <- function() {
  list("i"=c(0,3,7),
       "II"=c(1,5,8),
       "III"=c(3,7,10),
       "iv"=c(5,8,12),
       "v\u00B0"=c(7,10,13),
       "VI"=c(8,12,15),
       "vii"=c(10,13,17)
  )
}
locrian_tonic_chords <- function() {
  list("i\u00B0"=c(0,3,6),
       "II"=c(1,5,8),
       "iii"=c(3,6,10),
       "iv"=c(5,8,12),
       "V"=c(6,10,13),
       "VI"=c(8,12,15),
       "vii"=c(10,13,17)
  )
}
