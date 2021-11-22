#' partition_unit_table function
#' 
#' Separates a unit_table into a list of smaller unit_tables, 
#' one for each recording. 
#'
#' @param unit_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label. 
#'
#' @return A list of unit tables, one for each recording.
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' 
#' @export
#'
#' @examples unit_table = tibble::tibble(start = c(0.37, 0.6, 0.2, 1.8), end = c(0.45, 0.7, 0.4, 2), pos = c(1,2,1,2),sound.files = c("JS001.wav","JS001.wav","JS002.wav","JS002.wav") , note_label = "Curve")
#' partition_unit_table(unit_table)
partition_unit_table <- function(unit_table){
  
  #obtain vector of songs
  song_list = unit_table$sound.files %>% 
    unique()
  
  #make a tibble for each song
  song_tables = lapply(song_list, function(recording){
    unit_table %>% 
      dplyr::filter(sound.files == recording)
    })
  
  return(song_tables)
}
