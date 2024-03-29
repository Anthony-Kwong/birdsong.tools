% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_oscores_dep2.R
\name{compute_oscores_dep2}
\alias{compute_oscores_dep2}
\title{compute_oscores_dep2 function}
\usage{
compute_oscores_dep2(table1, table2)
}
\arguments{
\item{table1:}{A unit table tibble containing the original sound file, 
start/end times of the note,the note position and note_label.The soundfile 
must be the same as table2.}

\item{table2:}{A unit table tibble containing the original sound file, 
start/end times of the note,the note position and note_label. The soundfile 
must be the same as table1.}
}
\value{
A numeric vector with the sum of each of the 3 overlaps scores.
}
\description{
Dependency function for compute_oscores. Compares the overlap scores for table1
and table2, using table1 as a reference.
}
\examples{
table1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
sound.files = "JS001.wav", pos =c(1,2,3,4), note_label = "Curve")
table2 = tibble::tibble(start = c(0.4, 0.6, 1.5, 2.7), end = c(0.51, 0.7, 2.2, 3.2), 
sound.files = "JS001.wav", pos =c(1,2,3,4), note_label = "Curve")
compute_oscores_dep2(table1,table2)
}
