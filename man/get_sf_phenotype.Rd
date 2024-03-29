% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_sf_phenotype.R
\name{get_sf_phenotype}
\alias{get_sf_phenotype}
\title{get_sf_phenotype function}
\usage{
get_sf_phenotype(metadata, phenotype_table, phenotype_index)
}
\arguments{
\item{metadata:}{Dataframe containing Bird.ID and the IDs of their respective social fathers (Social.Father).}

\item{phenotype_table:}{Dataframe containing Bird.ID and some phenotype(s) of interest.}

\item{phenotype_index:}{Index of variable of interest in phenotype_table.}
}
\value{
: The phenotype table with an additional column for the social father's phenotype.
}
\description{
Adds the social father phenotype based on a phenotype table and a metadata table. If
the social father is NA (i.e. unknown), the value is NA. Function goes through each 
row in phenotype table and checks the father. Then we get the father's phenotype from the same
table.
}
\examples{
metadata = tibble::tibble(Bird.ID = c("JS01","JS02") , Social.Father = c("JS02",NA))
phenotype_table = tibble::tibble(Bird.ID = c("JS01","JS02"), trait = c(2,3))
get_sf_phenotype(metadata, phenotype_table,2)
}
