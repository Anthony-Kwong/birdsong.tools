#' get_sf_phenotype function
#' 
#' Adds the social father phenotype based on a phenotype table and a metadata table. If
#' the social father is NA (i.e. unknown), the value is NA.
#'
#' @param metadata: Dataframe containing Bird.ID and the IDs of their respective social fathers (Social.Father).
#' @param phenotype_table: Dataframe containing Bird.ID and some phenotype(s) of interest.
#' @param phenotype_index: Index of variable of interest in phenotype_table.
#' @return: The phenotype table with an additional column for the social father's phenotype.
#' @export
#'
#' @examples metadata = tibble::tibble(Bird.ID = c("JS01","JS02") , Social.Father = c("JS02",NA))
#' phenotype_table = tibble::tibble(Bird.ID = c("JS01","JS02"), trait = c(2,3))
#' get_sf_phenotype(metadata, phenotype_table,2)
get_sf_phenotype <- function(metadata, phenotype_table, phenotype_index){
  #initialise vector of sf phenotypes
  sf_phenotype = rep(NA,nrow(phenotype_table) )
  phenotypes = phenotype_table[,phenotype_index][[1]]
  
  #loop over each row in phenotype table
  for(i in 1:nrow(phenotype_table)){
    #find the row in metadata which corresponds to bird i in phenotype table
    bird_row = which(metadata$Bird.ID == phenotype_table$Bird.ID[i])
    #find corresponding social father of bird i
    sf = metadata$Social.Father[bird_row]
    
    if(length(bird_row) == 0){
      stop("BirdID in phenotype table not found in metadata")
    }
    
    #if no social father is recorded, return NA
    if(is.na(sf)){
      #sf_phenotype[i] = NA
      next
    }
    
    #take sf_phenotype based on phenotype_table
    sf_phenotype[i] =  phenotypes[which(phenotype_table$Bird.ID == sf)]
  }
  
  res = tibble::tibble(phenotype_table, sf_phenotype)
  return(res)
}