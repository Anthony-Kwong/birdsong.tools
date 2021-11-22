#' add_metadata function
#' 
#' Add the metadata to a dataframe based on a common ID column. 
#'
#' @param data : A dataframe with ID and some other columns 
#' @param metadata: A metadata table with ID
#' @param cols: Vector of column indices, indicating which columns of metadata to copy over.
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows
#'
#' @return The original dataframe with the additional columns from the 
#' metadata.
#' @export
#'
#' @examples data = tibble::tibble(Bird.ID = c("JS001", "JS002"),a = c(1,2), b = c(1,2))
#' metadata = tibble::tibble(Bird.ID = c("JS001", "JS002"), x= c(3,4), y = c(5,6), z = c(7,8))
#' add_metadata(data,metadata)
add_metadata <- function(data, metadata, cols){
  
  #check inputs
  if("Bird.ID" %in% colnames(data) == F){
    stop("Bird.ID column missing in data argument.")
  }
  
  if("Bird.ID" %in% colnames(metadata) == F){
    stop("Bird.ID column missing in metadata argument.")
  }
  
  if(is.numeric(cols) == F){
    stop("cols argument must be a vector of numeric integers.")
  }
  
  #check ID columns match
  check =  all(is.element(data$Bird.ID, metadata$Bird.ID))
  if(check == FALSE){
    stop("Mismatching IDs.")
  }
  
  #take variables from the metadata table based on Bird.ID
  meta_rows = lapply(as.list(data$Bird.ID), function(Bird.ID){
    row_index = which(metadata$Bird.ID == Bird.ID)
    meta_info = metadata[row_index, cols]
    meta_info$Bird.ID = NULL
    return(meta_info)
    #return(metadata)
  })
  
  #bind the new metadata columns to the main dataframe
  res = cbind(data, dplyr::bind_rows(meta_rows))
  
  return(res)
}
