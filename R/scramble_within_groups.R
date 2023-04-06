#' scramble_within_groups function
#' 
#' Permutes the rows of a dataframe within groups.
#'
#' @param data : A dataframe. One column is a categorical variable indicating group.
#' @param group_index : Column index of the variable indicating group.
#'
#' @return: A dataframe with permuted rows. Rows are only permuted within groups. Permutations are done via sampling with replacement.
#' @export
#'
#' @examples my_data <- data.frame(
#' Col1 = c(1, 2, 3, 4, 5, 6),
#' Col2 = c("A", "B", "A", "B", "C", "C"))
#' scramble_within_groups(my_data)
scramble_within_groups <- function(data, group_index) {
  
  #find the row indices of each group
  groups = unique(data[,group_index])
  
  group_pos = lapply(groups, function(g){
    which(data[,group_index] == g)
    
  })
  
  names(group_pos) <- groups
  
  #loop through rows and permute every one, output the indices of the permuted rows
  new_index = sapply(seq(nrow(data)), function(i){
    #get row of interest
    row = data[i,]
    #get group of interest
    g = row[,group_index]
    gdex = which(groups == g)
    #sample one row from the group, return the index
    new_i = sample(group_pos[[gdex]],1)
  })
  
  data_scrambled = data[new_index,]
  
  # # Split the data frame by groups in Col2
  # groups <- split(data, data[,group_index])
  # 
  # # Scramble the rows within each group
  # groups_scrambled <- lapply(groups, function(group) {
  #   sam_row = group[sample(nrow(group), replace = T),]
  #   })
  # 
  # # Combine the scrambled groups back into a data frame
  # data_scrambled <- do.call(rbind, groups_scrambled)
  # 
  # # Reorder the data frame to match the original order of rows
  # data_scrambled_ordered <- data_scrambled[order(match(rownames(data_scrambled), rownames(data))),]
  
  # Return the scrambled data frame
  return(data_scrambled)
}
