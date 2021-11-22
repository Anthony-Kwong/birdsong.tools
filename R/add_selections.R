#' add_selections function
#' 
#' Takes a unit table a adds a new column called "sel", indicating the number 
#' of the selected note in each song. (The k^th note would have sel k). Input unit table must
#' have columns: 1) "sound.files": name of the .wav files,
#' 2) "sel": number of the selections, 
#' 3) "start": start time of selections, 
#' 4) "end": end time of selections
#'
#' @param unit_table: A data.frame with the columns specified in the description. 
#'
#' @return A new unit table with a sel column.
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter arrange mutate
#' @importFrom data.table rbindlist
#' 
#' @export
#'
#' @examples unit_table = tibble::tibble(sound.files = c( rep("JS001.wav",3), rep("JS002.wav",3)),
#' start = seq(from = 1, to = 3.5, by = 0.5),
#' end = seq(from = 1, to = 3.5, by = 0.5) + 0.5)
add_selections <- function(unit_table){
  
  #check inputs
  
  #check all the correct columns are present
  correct_cols = c("start", "end", "sound.files")
  if(sum(colnames(unit_table) %in% correct_cols ) != length(correct_cols) ){
    stop("note argument does not have the correct columns. See documentation.")
  }
  
  #function starts here ----
  
  #obtain list of songs
  song_list = unit_table$sound.files %>% unique()
  
  #make a tibble for each song
  song_tables = lapply(song_list, function(song){
    unit_table %>%
      dplyr::filter(sound.files == song) %>%
      dplyr::arrange(start)
  })
  
  #add sel column
  updated_song_tables = lapply(song_tables, function(table){
    table %>%
      dplyr::mutate(sel = seq(nrow(table)))
  })
  
  #finally bind everything together
  res = data.table::rbindlist(updated_song_tables)
  
  return(res)
}