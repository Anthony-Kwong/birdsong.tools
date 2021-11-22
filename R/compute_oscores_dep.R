#' compute_oscores_dep
#' 
#' Dependency function for compute_oscores. Takes a note and matches it with
#' a note in a table with the most overlap. Computes the overlap scores between
#' the two notes. 
#'
#' @param note : 2-dimensional numeric vector. 
#' @param table :A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label. 
#'
#' @return A numeric vector containing the 3 overlap scores. 
#' @export
#'
#' @examples table1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
#' sound.files = "JS001.wav", pos =c(1,2,3,4), note_label = "Curve")
#' note = c(0.4,0.54)
compute_oscores_dep <- function(note, table){
  
  #get list of notes in table
  table_notes = split(table, row(table[,1]))
  #extract start and end points
  table_notelist = lapply(table_notes, function(table_row){
    note = c(table_row$start, table_row$end)
  })
  #compute overlaps
  olaps = sapply(table_notelist, function(man_note){
    check_overlap(note1 = man_note, note2 = c(note[1],note[2]))
  })
  #compute overlap scores with the best note
  x = which.max(olaps)
  res = overlap_score(note1 = unlist(table_notelist[x]),
                      note2 = c(note[1],note[2]))
  
  return(as.numeric(res))
}