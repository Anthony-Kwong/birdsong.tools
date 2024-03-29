% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overlap_score.R
\name{overlap_score}
\alias{overlap_score}
\title{overlap_score function}
\usage{
overlap_score(note1, note2)
}
\arguments{
\item{note1}{: A 2-dim numeric vector. Designated as y.}

\item{note2}{: A 2-dim numeric vector. Designated as z.}
}
\value{
A numeric vector for scores1,2,3.
}
\description{
Computes the overlap scores between 2 notes. The scores are defined as follows.
\eqn{score1 = (y \cap z) / y},
\eqn{score2 = (y \cap z) / (y \cup z)},
\eqn{score3 = |(y \cap z)|^2 / |y| \times |z|}
}
\examples{
note1 = c(1,5), note2 = c(3,4)
overlap_score(note1,note2)
}
