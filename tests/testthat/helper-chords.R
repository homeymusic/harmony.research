interval_components <- function() {
  tibble::tibble(
    integer_position = 0:12,
    name = c("Tonic","Minor 2nd","Major 2nd","Minor 3rd","Major 3rd",
             "Perfect 4th","Tritone","Perfect 5th","Minor 6th",
             "Major 6th","Minor 7th","Major 7th","Octave"),
    brightness =  c(1,-1,1,-1,1,-1,0,1,-1,1,-1,1,-1),
    affinity =    c(15,1,3,7,6,10,4,10,6,7,3,1,15)
  )
}
core_pitches <- function() {
  intervals = interval_components()
  dplyr::bind_rows(purrr::map2(intervals$integer_position,intervals$name,
                               ~h(.x,name=.y,observation_point = NA)))
}
major_triads <- function() {
  list(
    "root position"=h(c(0,4,7),
             root=0,
             observation_point=0,
             name="Major Triad\nRoot Position",
             midi_root=60),
    "1st inversion"=h(c(0,3,8),
                      root=8,
                      observation_point=12,
                      name="Major Triad\n1st Inversion",
                      midi_root=60-8),
    "2nd inversion"=h(c(0,5,9),
                      root=5,
                      observation_point=0,
                      name="Major Triad\n2nd Inversion",
                      midi_root=60-5)
  )
}
major_6_chords <- function() {
  list(
    "6/3"=h(c(0,3,8),
            root=0,
            observation_point=0,
            name="Major Triad 6/3",
            midi_root=60-0),
    "6/4"=h(c(0,5,9),
            root=0,
            observation_point=0,
            name="Major Triad 6/4",
            midi_root=60-0)
  )
}
minor_triads <- function() {
  list(
    "root position"=h(c(0,3,7),
             root=0,
             observation_point=0,
             name="Minor Triad\nRoot Position",
             midi_root=60-0),
    "1st inversion"=h(c(0,4,9),
                      root=9,
                      observation_point=12,
                      name="Minor Triad\n1st Inversion",
                      midi_root=60-9),
    "2nd inversion"=h(c(0,5,8),
                      root=5,
                      observation_point=0,
                      name="Minor Triad\n2nd Inversion",
                      midi_root=60-5)
  )
}
minor_6_chords <- function() {
  list(
    "minor 6/3"=h(c(0,4,9),
                  root=0,
                  observation_point=0,
                  name="Minor Triad 6/3",
                  midi_root=60-0),
    "minor 6/4"=h(c(0,5,8),
                  root=0,
                  observation_point=0,
                  name="Minor Triad 6/4",
                  midi_root=60-0)
  )
}
augmented_triads <- function() {
  list(
    'augmented_triad_up' = h(c(0,4,8),
                             root=0,
                             observation_point=0,
                             name='Augmented Triad Up'),
    'augmented_triad_down' = h(c(0,4,8),
                               root=8,
                               observation_point=12,
                               name='Augmented Triad Down',
                               midi_root=60-8)

  )
}
major_minor_triads <- function() {
  dplyr::bind_rows(
    dplyr::bind_rows(major_triads()),
    dplyr::bind_rows(major_6_chords()),
    dplyr::bind_rows(augmented_triads()),
    dplyr::bind_rows(minor_triads()),
    dplyr::bind_rows(minor_6_chords()),
  )
}
symmetrical_augmented_triads <- function() {
  list(
    'augmented_triad_up' = h(c(0,4,8),
                             root=0,
                             observation_point=0,
                             name='Augmented Triad Up'),
    'augmented_triad_down' = h(-c(0,4,8),
                               root=0,
                               observation_point=12,
                               name='Augmented Triad Down')

  )
}
phrygian_triads <- function() {
  list(
    "root position"=h(-c(0,4,7),
                      root=0,
                      observation_point=12,
                      name="Phrygian Triad\nRoot Position",
                      midi_root=60),
    "1st inversion"=h(-c(0,3,8),
                      root=-8,
                      observation_point=0,
                      name="Phrygian Triad\n1st Inversion",
                      midi_root=60+8),
    "2nd inversion"=h(-c(0,5,9),
                      root=-5,
                      observation_point=12,
                      name="Phrygian Triad\n2nd Inversion",
                      midi_root=60+5)
  )
}
phrygian_6_chords <- function() {
  list(
    "6/3"=h(-c(0,3,8),
            root=0,
            observation_point=12,
            name="Phrygian Triad 6/3",
            midi_root=60+0),
    "6/4"=h(-c(0,5,9),
            root=0,
            observation_point=12,
            name="Major Triad 6/4",
            midi_root=60+0)
  )
}
major_phrygian_triads <- function() {
  dplyr::bind_rows(
    dplyr::bind_rows(major_triads()),
    dplyr::bind_rows(major_6_chords()),
    dplyr::bind_rows(symmetrical_augmented_triads()),
    dplyr::bind_rows(phrygian_triads()),
    dplyr::bind_rows(phrygian_6_chords())
  )
}
seventh_chords <- function() {
  list(
    "Major"=h(c(0,4,7,11),
              root=0,
              observation_point=0,
              name="Major"),
    "Dominant Flat Five"=h(c(0,4,6,10),
                           root=0,
                           observation_point=0,
                           name="Dominant Flat Five"),
    "Dominant"=h(c(0,4,7,10),
                 root=0,
                 observation_point=0,
                 name="Dominant"),
    "Augmented"=h(c(0,4,8,10),
                  root=0,
                  observation_point=0,
                  name="Augmented"),
    "Augmented Major"=h(c(0,4,8,11),
                        root=0,
                        observation_point=0,
                        name="Augmented Major"),
    "minor"=h(c(0,3,7,10),
              root=0,
              observation_point=0,
              name="Minor"),
    "minor major"=h(c(0,3,7,11),
                    root=0,
                    observation_point=0,
                    name="Minor-Major"),
    "half-diminished"=h(c(0,3,6,10),
                        root=0,
                        observation_point=0,
                        name="Half-Diminished"),
    "diminished major"=h(c(0,3,6,11),
                         root=0,
                         observation_point=0,
                         name="Diminished Major"),
    "diminished"=h(c(0,3,6,9),
                   root=0,
                   observation_point=0,
                   name="Diminished")
  )
}
diatonic_scales <- function() {
  list(
    'locrian'=h(c(0,1,3,5,6,8,10,12), name = 'Locrian'),
    'phrygian'=h(c(0,1,3,5,7,8,10,12), name = 'Phrygian'),
    'aeolian'=h(c(0,2,3,5,7,8,10,12), name = 'Aeolian'),
    'dorian'=h(c(0,2,3,5,7,9,10,12), name = 'Dorian'),
    'mixolydian'=h(c(0,2,4,5,7,9,10,12), name = 'Mixolydian'),
    'ionian'=h(c(0,2,4,5,7,9,11,12), name = 'Ionian'),
    'lydian'=h(c(0,2,4,6,7,9,11,12), name = 'Lydian')
  )
}
major_triad_progression <- function(){
  major_triad = c(0,4,7)
  list(
    h(major_triad  , observation_point=0),
    h(major_triad+5, observation_point=0),
    h(major_triad+7, observation_point=0),
    h(major_triad  , observation_point=0)
  )
}
voice_leading_progression <- function(){
  major_triad = c(0,4,7)
  list(
    h(major_triad  , observation_point=0),
    h(major_triad+1, observation_point=0),
    h(major_triad+2, observation_point=0),
    h(major_triad+3, observation_point=0),
    h(major_triad+4, observation_point=0),
    h(major_triad+5, observation_point=0),
    h(major_triad+6, observation_point=0),
    h(major_triad+7, observation_point=0),
    h(major_triad+8, observation_point=0),
    h(major_triad+9, observation_point=0),
    h(major_triad+10, observation_point=0),
    h(major_triad+11, observation_point=0),
    h(major_triad+12, observation_point=0),
    h(major_triad+13, observation_point=0),
    h(major_triad  , observation_point=0)
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
  list("I"         =h(c( 0, 4, 7),observation_point=0,name='I'),
       "ii"        =h(c( 2, 5, 9),observation_point=0,name='ii'),
       "iii"       =h(c( 4, 7,11),observation_point=0,name='iii'),
       "IV"        =h(c( 5, 9,12),observation_point=0,name='IV'),
       "V"         =h(c( 7,11,14),observation_point=0,name='V'),
       "vi"        =h(c( 9,12,16),observation_point=0,name='vi'),
       "vii\u00B0" =h(c(11,14,17),observation_point=0,name='vii\u00B0'),
       "VIII"      =h(c( 0, 4, 7)+12,observation_point=0,name='VIII')
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
salzer_schachter_1.1.a <- function() {
  list(h(0),
       h(2),
       h(0),
       h(-1),
       h(0),
       h(2),
       h(0))
}
salzer_schachter_1.1.b <- function() {
  list(h(0),
       h(2),
       h(5),
       h(4),
       h(2),
       h(5),
       h(4),
       h(5),
       h(4),
       h(2),
       h(0))
}
salzer_schachter_1.1.c <- function() {
  list(h(0),
       h(2),
       h(5),
       h(4),
       h(9),
       h(7),
       h(5),
       h(2),
       h(4),
       h(2),
       h(0)
  )
}
salzer_schachter_1.2 <- function() {
  list(h(0),
       h(9),
       h(4),
       h(11),
       h(7),
       h(16),
       h(12),
       h(9),
       h(11),
       h(7),
       h(2),
       h(0))
}
salzer_schachter_1.3 <- function() {
  list(h(0),
       h(2),
       h(3),
       h(5),
       h(7),
       h(8),
       h(7),
       h(5),
       h(3),
       h(2),
       h(0))
}

major_triad_root = major_triads()[["root position"]]
major_triad_first_inversion = major_triads()[["1st inversion"]]
major_triad_second_inversion = major_triads()[["2nd inversion"]]

minor_triad_root = minor_triads()[["root position"]]
minor_triad_first_inversion = minor_triads()[["1st inversion"]]
minor_triad_second_inversion = minor_triads()[["2nd inversion"]]

locrian = diatonic_scales()[['locrian']]
