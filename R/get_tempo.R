#' get_tempo function
#' 
#' Takes a unit table and computes the number of notes/sec (tempo) for
#' each recording. 
#'
#' @param unit_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label.  
#'
#' @return A 2 column dataframe containing each of the recording and its tempo. 
#' @export
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#'
#' @examples unit_table = tibble::tibble(start = c(0.37, 0.6, 0.2, 1.8), end = c(0.45, 0.7, 0.4, 2), pos = c(1,2,1,2),sound.files = c("JS001.wav","JS001.wav","JS002.wav","JS002.wav") , note_label = "Curve")
#' get_tempo(unit_table)
get_tempo <- function(unit_table){
  
  song_tables = partition_unit_table(unit_table)
  
  res_tables = lapply(song_tables, function(note_table){
    song_len = note_table$end[nrow(note_table)] - note_table$start[1]
    tempo = nrow(note_table)/ song_len
    tibble::tibble(sound.files = note_table$sound.files[1],tempo)
  })
  
  res = dplyr::bind_rows(res_tables)
  
  return(res)
}
