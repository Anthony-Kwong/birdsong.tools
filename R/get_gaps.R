#' get_gaps function
#' 
#' Takes a unit table of notes and generates a corresponding
#' unit table for its gaps. 
#'
#' @param unit_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label.  
#'
#' @return A dataframe with the recording, position of gap and the gap duration. 
#' @export
#' 
#' @importFrom tibble tibble
#'
#' @examples unit_table = tibble::tibble(start = c(0.37, 0.6, 0.75, 0.2, 1.8, 2.5), end = c(0.45, 0.7, 0.9, 0.4, 2, 3), selec = c(1,2,3,1,2,3),sound.files = c(rep("JS001.wav",3),rep("JS002.wav", 3)), note_label = "Curve")
#' get_gaps(unit_table)
get_gaps <- function(unit_table){
  
  #check columns

  correct_cols = c("start", "end", "selec", "sound.files", "note_label")
  if(sum(colnames(unit_table) %in% correct_cols ) != length(correct_cols) ){
    stop("Incorrect columns. See documentation")
  }
  
  #separate the unit table by recording
  song_tables = partition_unit_table(unit_table)
  
  # #testing
  # lapply(song_tables, function(note_table){
  #   gap_dur = rep(NA, nrow(note_table) - 1)
  #   
  #   for(i in 2:nrow(note_table)){
  #     gap_dur[i-1] = note_table$start[i] - note_table$end[i-1]
  #   }
  #   #return(gap_dur)
  #   trans = add_transitions(note_table)$transitions[1:nrow(note_table)-1]
  #   #need to add note classes!
  # })
  
  #compute gaps for each unit table
  gap_tables = lapply(song_tables, function(note_table){
    #initialise vector for gap lengths
    gap_dur = rep(NA, nrow(note_table) - 1)
    #loop over all rows starting with the second
    for(i in 2:nrow(note_table)){
      gap_dur[i-1] = note_table$start[i] - note_table$end[i-1]
    }
    #add transitions column
    trans = add_transitions(note_table)$transitions[1:nrow(note_table)-1]
    res = tibble::tibble( sound.files = note_table$sound.files[1], 
                          gap_dur, selec = seq(length(gap_dur)),
                          transitions = trans)
    return(res)
  })
  
  results = dplyr::bind_rows(gap_tables)
   
  return(results)
}
