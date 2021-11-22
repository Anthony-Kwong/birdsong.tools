#' Subfunction for add_transitions
#' 
#' Used in add_transitions() to add a transitions column for an ordered
#' unit table from one song. Designed only for use within add_transitions(). 
#'
#' @param table: Ordered unit table of notes from one song. 
#'
#' @return A unit table with a transitions column. 
#' @export
#'
#' @examples unit_table = tibble::tibble(start = c(0.37, 0.6, 0.2, 1.8), end = c(0.45, 0.7, 0.4, 2), selec = c(1,2,3,4),sound.files ="JS001.wav" , note_label = "Curve")
#' add_transitions_dep(unit_table)
add_transitions_dep <- function(table){

  transitions = rep(NA, nrow(table))

  for(i in 1:nrow(table)){
    if(i == nrow(table)){
      transitions[i] = "end"
    } else {
      transitions[i] = paste(table$note_label[i],"-", table$note_label[i + 1],sep="")
    }
  }
  
  res = cbind(table, transitions)
  
  return(res)
}
