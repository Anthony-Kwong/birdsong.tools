#' get_var_score function
#' 
#' Takes a gap_table for an individual and computes the overall variability score
#' for an individual bird. 
#' 
#' The function first sorts the gaps by transition type and computes a score for each transition. This 
#' is the variance of the gaps, divided by their mean. The final score is a linear combination of the 
#' scores for each transition. The constant for the score for transition type AB is the total number 
#' AB transitions divided by the total number of transitions - the number of transition types. 
#'  
#' The score is the variance of all the gaps, divided by their mean. 
#'
#' @param gap_table: A tibble as produced using the get_gaps function. It has Bird.ID, gap_dur,
#' selec and transitions. 
#'
#' @return A numeric, scalar score. 
#' @export
#'
#' @examples unit_table = tibble::tibble(start = c(0.37, 0.6, 0.75, 0.2, 1.8, 2.5), 
#' end = c(0.45, 0.7, 0.9, 0.4, 2, 3), 
#' selec = c(1,2,3,1,2,3),
#' sound.files = c(rep("JS001.wav",6)), 
#' note_label = rep(c("A","B","C"),2))
#' output = get_gaps(unit_table)
#' get_var_score_dep(output)
gap_var_score <- function(gap_table){
  #this needs to pool all the recordings for each bird
  
  #get all available transitions for the bird across all its recordings
  trans = unique(gap_table$transitions)
  #compute scores across all transition types
  scores = sapply(trans, function(transition){
    trans_table = dplyr::filter(gap_table, transitions == transition)
    #compute the var score for that transition type
    score = get_var_score_dep(trans_table)
    #record the number of occurences for that transition
    count = nrow(trans_table)
    #res = tibble::tibble(score = score, count = count)
    res = c(score = score, count = count)
    return(res)
  })
  #var score for each transition type
  var_scores = scores[1,]
  #counts of each transition type
  trans_counts = scores[2,]
  #total number of transitions
  total_trans = nrow(gap_table)
  #number of difference transition types
  num_trans = length(trans)
  
  a = rep(NA,num_trans)
  #lin combination of the scores
  lc_score = rep(NA,num_trans)
  #compute constants
  for(i in 1:num_trans){
    #a is recorded here for ease of debugging
    a[i] = trans_counts[i]/(total_trans - num_trans)
    lc_score[i] = a[i]*var_scores[i]
  }
  
  #compute final score
  score = sum(lc_score)

  return(score)
}