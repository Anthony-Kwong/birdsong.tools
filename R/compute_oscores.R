#' compute_oscores function
#' 
#' Compares a computational unit table against a manual unit table using the 
#' overlap scores defined in overlap_score. Function attempts to match 
#' each computational note to a note in the manual table with the most 
#' overlap.
#' 
#'
#' @param manual_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label.
#' @param comp_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label. 
#'
#' @return A numeric vector containing the totals of the 3 overlaps scores. 
#' @export
#' 
#' @importFrom purrr map2
#' @importFrom dplyr bind_rows
#'
#' @examples table1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
#' sound.files = c(rep("JS001.wav",2),rep("JS002.wav",2)) , pos =c(1,2,3,4), note_label = "Curve")
#' table2 = tibble::tibble(start = c(0.25, 0.65, 1.6, 2.4), end = c(0.45, 0.75, 2.1, 2.9), 
#' sound.files = c(rep("JS001.wav",2),rep("JS002.wav",2)), pos =c(1,2,3,4), note_label = "Unclassifed")
#' compute_oscores(table1,table2)
compute_oscores = function(manual_table, comp_table){
  
  #ensure recordings are in the same order for both tables
  #we are relying on both input tables already having the same set of recordings
  manual_table = manual_table[order(manual_table$sound.files),]
  comp_table = comp_table[order(comp_table$sound.files),]
  
  #split tables by recording
  truth_tables = partition_unit_table(manual_table)
  comp_tables = partition_unit_table(comp_table)
  
  #compute scores for each table pair
  table_scores = purrr::map2(.x = truth_tables, .y = comp_tables, .f = compute_oscores_dep2)
  #bind everything into one table
  score_table = dplyr::bind_rows(table_scores)
                                      
  #sum across all the tables
  final_scores = colSums(score_table)
  
  return(final_scores)
}
