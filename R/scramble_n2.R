#' scramble n2 function
#' 
#' Takes the IOI table (output of compute_IOI) and permutes note2 (along with t2) according to
#' their note classes. I.e. Curve notes are replaced by other curve notes within the same song
#'
#' @param ioi : An IOI table output by compute_IOI
#'
#' @return List of 2. 1:A permuted IOI table where each note2 is replaced by another note of the same class. 
#' 2: Vector of row indices for the permuted IOI table
#' @importFrom tibble tibble
#' @export
#' 
#' 
#'
#' @examples unit_table = tibble::tibble(start = c(0.37, 0.6, 1.2, 1.9), end = c(0.45, 0.7, 1.4, 2),
#' pos = seq(1,4),sound.files = "JS001.wav" , note_label = rep(c("A","B"),2))
#' ioi = compute_IOI(unit_table)
#' perm_ioi = scramble_n2(ioi)
scramble_n2 <- function(ioi){
  #obtain note types to scramble
  note_types = unique(ioi$note2)
  note_types = as.character(na.omit(note_types))
  
  #slice dataframe columns for permutation 
  slice_data = ioi[,4:6]
  
  #loop for each note type, output permuted indices
  perm_row_indices = lapply(note_types, function(n){
    #for note type n, do:
    
    #get indices of every row where note2=n
    ori_index = which(ioi$note2 == n)
    
    #control for cases where ori_index only has one element. Sample(5) would give a random sample from the first 5 integers
    if(length(ori_index) == 1){
      return(ori_index)
    } else {
      #permute the indices
      perm_index = sample(ori_index, replace = T)
      return(perm_index)
    }
  })
  
  #bug found. This only works if the note types are all in order. e.g. A's first...then B's... then C's
  new_rows = unlist(perm_row_indices)
  
  #permute rows of sliced data
  perm_rows = slice_data[new_rows,]
  
  #replace original IOI table with new note2's and t2's, remove the last row since it cannot be used to make r
  res = tibble::tibble(ioi[1:nrow(ioi) - 1,1:3], perm_rows)
  
  return(list(res, new_rows))  
}