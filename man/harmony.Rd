% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmony.R
\name{harmony}
\alias{harmony}
\alias{h}
\title{Harmony}
\usage{
harmony(
  chord,
  observation_point = NA,
  root = NA,
  name = NA,
  midi_reference = NA,
  default_consonance_metric = "stolzenburg2015",
  include_label = TRUE
)

h(
  chord,
  observation_point = NA,
  root = NA,
  name = NA,
  midi_reference = NA,
  default_consonance_metric = "stolzenburg2015",
  include_label = TRUE
)
}
\arguments{
\item{chord}{A pitch or chord expressed as an interval integer or vector of interval integers}

\item{observation_point}{Harmonic observation_point 0 is tonic, 12 is octave, NA is symmetrical}

\item{root}{The reference pitch of the chord or larger context}

\item{name}{A custom name for the note or chord}

\item{midi_reference}{The MIDI root of the chord, defaults to 60 for using 0-based integer notation}

\item{default_consonance_metric}{The metric that will populate consonance and brightness values}

\item{include_label}{Helpful for creating plots what close the loop of a path}
}
\value{
A tibble
}
\description{
Provides the harmonic metrics of a note or chord.
}
