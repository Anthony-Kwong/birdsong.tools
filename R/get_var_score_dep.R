#' get_var_score_dep function
#' 
#' Dependency function for get_var_score. Not to be used as a standalone. Takes a gap_table for gaps
#' belonging to the same transition type and computes the variability score. The score is the standard
#' deviation of all the gaps, divided by their mean. 
#'
#' @param gap_table: A tibble as produced using the get_gaps function. It has columns sound.files, gap_dur,
#' selec and transitions. 
#' @param denom_var: logical indicating whether the standard denominator for variance N-1 should be used 
#' instead of N. T is for N-1, F for N. Default is N-1. 
#' @param min: The minimum number of transitions required for computing the bird's individual Cjb (coefficient of variation).
#' If the bird sings less of a transition than min, the coefficient is 0. Default min = 2.
#'
#' @return A numeric, scalar score. 
#' @export
#'
#' @examples unit_table = tibble::tibble(start = c(0.37, 0.6, 0.75, 0.2, 1.8, 2.5), 
#' end = c(0.45, 0.7, 0.9, 0.4, 2, 3), 
#' selec = c(1,2,3,1,2,3),
#' sound.files = c(rep("JS001.wav",3),rep("JS002.wav", 3)), 
#' note_label = rep(c("A","B","C"),2))
#' output = get_gaps(unit_table)
#' get_var_score_dep(output)
get_var_score_dep <- function(gap_table, denom_var = T, min = 2){
  #check gaps are all of the one transition type
  trans_types = unique(gap_table$transitions)
  if(length(trans_types) > 1){
    msg = "More than one transition type given."
    stop(msg)
  }
  
  #account for case when there is only one occurrence of a transition
  if(nrow(gap_table) < min){
    msg = paste("Only one occurrence of transition ", gap_table$transitions[1], " found in recording ",
                gap_table$sound.files[1], " found. Setting variance score for that transition for that individual to 0.")
    warning(msg)
    return(0)
  }
  
  #get number of occurences
  N = nrow(gap_table)
  #choice to use the version of std with N denominator rather than N-1
  if(denom_var){
    score = sd(gap_table$gap_dur)
  } else {
    score = sqrt(var(gap_table$gap_dur)*(N-1)/N)
  }
  
  #normalize the score using the bird's mean
  norm_score = score/mean(gap_table$gap_dur)
  
  return(norm_score)
}