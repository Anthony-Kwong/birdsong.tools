#' get_duration function
#' 
#' Computes the duration of a song using its unit table. 
#'
#' @param unit_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label.   
#'
#' @return: A numeric scalar, the end of the first note minus the start of the first note.
#' @export
#'
#' @examples   unit_table = tibble::tibble(start = c(0.37, 0.6, 0.75, 0.2, 1.8, 2.5),
#'end = c(0.45, 0.7, 0.9, 0.4, 2, 3), 
#'sound.files = "JS001.wav",
#'select = seq(6),
#'note_label = "Curve")
#'get_duration(unit_table)
get_duration <- function(unit_table){
  #check songs are all from the one recording
  if( length( unique(unit_table$sound.files) ) > 1){
    msg = "Only one recording allowed in get_duration."
    stop(msg)
  }
  
  #song begins at the start of the first note
  start = min(unit_table$start)
  #song ends at the end of the last note
  end = max(unit_table$end)
  
  #duration is the distance between the end and the start
  dur = end - start
  
  return(dur)
}