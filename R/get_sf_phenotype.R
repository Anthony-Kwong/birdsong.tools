#' get_sf_phenotype function
#' 
#' Adds the social father phenotype based on a phenotype table and a metadata table. If
#' the social father is NA (i.e. unknown), the value is NA. Function goes through each 
#' row in phenotype table and checks the father. Then we get the father's phenotype from the same
#' table. 
#'
#' @param metadata: Dataframe containing Bird.ID and the IDs of their respective social fathers (Social.Father).
#' @param phenotype_table: Dataframe containing Bird.ID and some phenotype(s) of interest.
#' @param phenotype_index: Index of variable of interest in phenotype_table.
#' @return: The phenotype table with an additional column for the social father's phenotype.
#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' @export
#'
#' @examples metadata = tibble::tibble(Bird.ID = c("JS01","JS02") , Social.Father = c("JS02",NA))
#' phenotype_table = tibble::tibble(Bird.ID = c("JS01","JS02"), trait = c(2,3))
#' get_sf_phenotype(metadata, phenotype_table,2)
get_sf_phenotype <- function(metadata, phenotype_table, phenotype_index){
  #initialise vector of sf phenotypes
  sf_phenotype = rep(NA,nrow(phenotype_table) )
  #take vector of phenotypes from the phenotype table
  phenotypes = dplyr::pull(phenotype_table, phenotype_index)
  #save the class
  var_class = class(phenotypes)
  
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
    #print(i)
    #take sf_phenotype based on phenotype_table
    
    #account for how R stores dates differently
    if(class(phenotypes)=="Date"){
      #format(as.Date("11-Jan-2011",format="%d-%b-%Y"), "%m/%d/%y")
      sf_phenotype[i] = format( as.Date(phenotypes[which(phenotype_table$Bird.ID == sf)], format="%d/%b/%Y") )
    } else {
      sf_phenotype[i] =  phenotypes[which(phenotype_table$Bird.ID == sf)]
    }
  }
  #turn sf_phenotype back into a vector
  sf_phenotype_vec = unlist(sf_phenotype)
  
  #change dates back into dates
  if(var_class=="Date"){
    res = tibble::tibble(phenotype_table, sf_phenotype = as.Date(sf_phenotype))
    return(res)
  }
  
  res = tibble::tibble(phenotype_table, sf_phenotype = sf_phenotype_vec)
  return(res)
}