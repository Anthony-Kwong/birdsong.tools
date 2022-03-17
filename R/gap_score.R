#' gap_score function
#' 
#' Computes the gap score for a bird, using the unit table derived from all its recordings. 
#' 
#' For each transition type AB, compute
#' 
#' \deqn{score_AB = log( individual mean AB gaps / population mean AB gaps )}
#' 
#' The final score is the average of the scores for every transition type in the individual bird. 
#' 
#' @param gap_table: A tibble as produced using the get_gaps function. It has Bird.ID, gap_dur,
#' selec and transitions.  
#' @param pop_means: A tibble containing the mean gap lengths for each transition type. Columns should be
#' transitions and mean_gap. 
#'
#' @return
#' @export
#'
#' @examples    unit_table = tibble::tibble(start = c(0.37, 0.6, 0.75, 1.2, 1.8, 2.5), 
#' end = c(0.45, 0.7, 0.9, 1.4, 2, 3), 
#' selec = seq(6),
#' sound.files = "JS001.wav", 
#' note_label = rep(c("A","B","C"),2))
#' gap_table = get_gaps(unit_table)
#' gap_score(gap_table)
gap_score <- function(gap_table, pop_means){
  
  #obtain all transition types
  trans = unique(gap_table$transitions)
  
  #group gaps by transition type
  means = lapply(trans, function(trans_type){
    #filter data for transition
    data = dplyr::filter(gap_table, transitions == trans_type)
    #compute mean gap length
    score = mean(data$gap_dur)
    res = tibble::tibble(mean = score, transitions = trans_type)
    return(res)
  })
  
  #collect individual means for each transition
  ind_means = do.call(rbind, means)
  
  #compute log scores
  scores = lapply( trans, function(trans){
    index = which(ind_means$transitions == trans)
    ind_value = ind_means$mean[index]
    
    index = which(pop_means$transitions == trans)
    pop_value = pop_means$mean_gap[index]
    
    res = log(ind_value/pop_value)
  } )
  
  #average up score over each transition type
  final_score = mean( unlist(scores) )
  return(final_score)
}