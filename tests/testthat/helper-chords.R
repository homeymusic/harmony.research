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
    "root position"=h(c(0,4,7)%>%sort,
             root=0,
             observation_point=0,
             name="Major Triad\nRoot Position"),
    "1st inversion"=h(c(0+12,4,7)%>%sort,
                      root=0+12,
                      observation_point=12,
                      name="Major Triad\n1st Inversion"),
    "2nd inversion"=h(c(0+12,4+12,7)%>%sort,
                      root=0+12,
                      observation_point=0,
                      name="Major Triad\n2nd Inversion")
  )
}
major_6_chords <- function() {
  list(
    "6/3"=h(c(0+12,4,7)%>%sort,
            root=4,
            observation_point=0,
            name="Major 6/3"),
    "6/4"=h(c(0+12,4+12,7)%>%sort,
            root=7,
            observation_point=0,
            name="Major 6/4")
  )
}
minor_triads <- function() {
  list(
    "root position"=h(c(0,3,7)%>%sort,
             root=0,
             observation_point=0,
             midi_reference = 60 + 9,
             name="Minor Triad\nRoot Position"),
    "1st inversion"=h(c(0+12,3,7)%>%sort,
                      root=0+12,
                      observation_point=12,
                      midi_reference = 60 + 9,
                      name="Minor Triad\n1st Inversion"),
    "2nd inversion"=h(c(0+12,3+12,7)%>%sort,
                      root=12,
                      observation_point=0,
                      midi_reference = 60 + 9,
                      name="Minor Triad\n2nd Inversion")
  )
}
minor_6_chords <- function() {
  list(
    "minor 6/3"=h(c(0+12,3,7)%>%sort,
                  root=3,
                  observation_point=0,
                  midi_reference = 60 + 9,
                  name="Minor 6/3"),
    "minor 6/4"=h(c(0+12,3+12,7)%>%sort,
                  root=7,
                  observation_point=0,
                  midi_reference = 60 + 9,
                  name="Minor 6/4")
  )
}
augmented_triads <- function() {
  list(
    'augmented_triad_up' = h(c(0,4,8),
                             root=0,
                             observation_point=0,
                             midi_reference = 60 + 6,
                             name='Augmented Triad Up'),
    'augmented_triad_down' = h(c(0,4,8),
                               root=8,
                               observation_point=12,
                               midi_reference = 60 + 6,
                               name='Augmented Triad Down')

  )
}
augmented_triad <- function() {
  h(c(0,4,8),
    observation_point = NA,
    name='Augmented Triad')
}
max_dissonance_triad <- function() {
  h(c(0,1,11),observation_point = NA,name='Max Dissonance Triad')
}
major_minor_triads <- function() {
  dplyr::bind_rows(
    dplyr::bind_rows(major_triads()),
    dplyr::bind_rows(major_6_chords()),
    dplyr::bind_rows(minor_triads()),
    dplyr::bind_rows(minor_6_chords()),
    augmented_triad(),
    max_dissonance_triad()
  )
}
symmetrical_augmented_triads <- function() {
  list(
    'augmented_triad_up' = h(c(0,4,8),
                             root=0,
                             observation_point=0,
                             midi_reference = 60 + 6,
                             name='Augmented Triad Up'),
    'augmented_triad_down' = h((-c(0,4,8))%>%sort,
                               root=0,
                               observation_point=12,
                               midi_reference = 60 + 6,
                               name='Augmented Triad Down')

  )
}
phrygian_triads <- function() {
  list(
    "root position"=h((-c(0,4,7))%>%sort,
                      root=0,
                      observation_point=12,
                      midi_reference = 60 + 4,
                      name="Dual Minor Triad\nRoot Position"),
    "1st inversion"=h((-c(0+12,4,7))%>%sort,
                      root=-12,
                      observation_point=0,
                      midi_reference = 60 + 4,
                      name="Dual Minor Triad\n1st Inversion"),
    "2nd inversion"=h((-c(0+12,4+12,7))%>%sort,
                      root=-12,
                      observation_point=12,
                      midi_reference = 60 + 4,
                      name="Dual Minor Triad\n2nd Inversion")
  )
}
phrygian_6_chords <- function() {
  list(
    "6/3"=h((-c(0+12,4,7))%>%sort,
            root=-4,
            observation_point=12,
            name="Dual Minor 6/3",
            midi_reference=60+4),
    "6/4"=h((-c(0+12,4+12,7))%>%sort,
            root=-7,
            observation_point=12,
            name="Dual Minor 6/4",
            midi_reference=60+4)
  )
}
major_phrygian_triads <- function() {
  dplyr::bind_rows(
    dplyr::bind_rows(major_triads()),
    dplyr::bind_rows(major_6_chords()),
    dplyr::bind_rows(phrygian_triads()),
    dplyr::bind_rows(phrygian_6_chords()),
    augmented_triad(),
    max_dissonance_triad()
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
    'locrian'=h(c(0,1,3,5,6,8,10,12), name = 'B Locrian', midi_reference = 60 + 11),
    'phrygian'=h(c(0,1,3,5,7,8,10,12), name = 'E Phrygian', midi_reference = 60 + 4),
    'aeolian'=h(c(0,2,3,5,7,8,10,12), name = 'A Aeolian', midi_reference = 60 + 9),
    'dorian'=h(c(0,2,3,5,7,9,10,12), name = 'D Dorian', midi_reference = 60 + 2),
    'mixolydian'=h(c(0,2,4,5,7,9,10,12), name = 'G Mixolydian', midi_reference = 60 + 7),
    'ionian'=h(c(0,2,4,5,7,9,11,12), name = 'C Ionian', midi_reference = 60 + 0),
    'lydian'=h(c(0,2,4,6,7,9,11,12), name = 'F Lydian', midi_reference = 60 + 5)
  )
}
diatonic_triads <- function() {
  list(
    'ionian'     =h(c(0,4,7),
                    name = 'C Ionian P5/M3 I<IV<V', midi_reference = 60 + 0),
    'phrygian'   =h(-c(0,4,7),
                    name = 'E Phrygian -P5/-M3 viii>v>iv', midi_reference = 60 + 4,
                    observation_point = OCTAVE),
    'mixolydian' =h(-c(0,3,7),
                    name = 'G Mixolydian -P5/-m3 VIII>V>IV', midi_reference = 60 + 7,
                    observation_point = OCTAVE),
    'aeolian'    =h(c(0,3,7),
                    name = 'A Aeolian P5/m3 i<iv<v', midi_reference = 60 + 9),
    'dorian up'  =h(c(0,5,9),
                    name = 'D Dorian Up M6/P4 I<IV<VII', midi_reference = 60 + 2),
    'dorian down'=h(-c(0,5,9),
                    name = 'D Dorian Down M6/P4 viii>v>i', midi_reference = 60 + 2,
                    observation_point = OCTAVE),
    'locrian'    =h(c(0,3,8),
                    name = 'B Locrian m6/m3 I<IV<VII', midi_reference = 60 + 11),
    'lydian'     =h(-c(0,3,8),
                    name = 'F Lydian -m6/-m3 viii>v>i', midi_reference = 60 + 5,
                    observation_point = OCTAVE)
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
