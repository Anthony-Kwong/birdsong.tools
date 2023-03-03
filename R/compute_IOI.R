#' compute_IOI
#'
#' Takes a unit table and computes the inter-onset-intervals for a single sound file. 
#'
#' @param unit_table:  A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label.  
#'
#' @return: A dataframe containing the ID, recording and IOIs of the start and end notes, note classes and the order of the ratio in the recording.
#' @importFrom tibble tibble
#' @export
#'
#' @examples  unit_table = tibble::tibble(start = c(0.37, 0.6, 1.2, 1.9), end = c(0.45, 0.7, 1.4, 2),
#' pos = seq(1,4),sound.files = "JS001.wav" , note_label = rep(c("A","B"),2))
#' compute_IOI(unit_table)
compute_IOI <- function(unit_table){
  #check unit table only contains notes from one recording
  if( length( unique(unit_table$sound.files) ) > 1){
    msg = "Unit table must come from one recording only"
    stop(msg)
  }
  
  #number of rows in output table is 1 less because we don't get the ratio on the final note
  n = nrow(unit_table) - 1
  
  #compute IOI of the starting note
  tk = diff(unit_table$start)
  #compute IOI for ending note, last element is NA because we can't compute IOI for the very last note.
  tl = c(tk[2:n], NA)
  end_note = c(unit_table$note_label[2:n], NA)
  
  res = tibble::tibble(sound.files = unit_table$sound.files[1:n], t1 = tk, note1 = unit_table$note_label[1:n],
                       t2 = tl, note2 = end_note, pos = seq(n))
  return(res)
}