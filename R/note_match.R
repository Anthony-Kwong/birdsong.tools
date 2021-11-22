#' note_match function
#' 
#' Takes a table of computationally excised notes and matches it to a table of
#' manually classified notes. Notes are considered matching if the midpoint 
#' of the computationally excised note is within the interval of a manually
#' classified note. 
#'
#' @param comp_table: A tibble of computationally excised notes containing the 
#' original sound file, start/end times of the note. 
#' @param manual_table: A tibble of manually classified notes containing the 
#' original sound file, start/end times of the note and the note class.
#'
#' @return A vector of note classes for comp_table. The class is unclassified if
#' no matching note is found. 
#' 
#' @importFrom data.table between
#' @importFrom dplyr filter
#' 
#' @export
#'
#' @examples comp_table = tibble(start = c(0.35, 0.55), end = c(0.49, 0.7), sound.files = "JS001.wav")
#' manual_table = tibble(start = c(0.37, 0.6), end = c(0.45, 0.7), sound.files = "JS001.wav", note_label = "Curve")
#' note_match(comp_table, manual_table)
note_match <- function(comp_table, manual_table){
  
  #check all the correct columns are present
  correct_cols = c("start", "end", "sound.files")
  if(sum(colnames(comp_table) %in% correct_cols ) != length(correct_cols) ){
    stop("comp_table does not have the correct columns. See documentation.")
  }
  
  correct_cols = c("start", "end", "sound.files", "note_label")
  if(sum( colnames(manual_table) %in% correct_cols ) != length(correct_cols) ){
    stop("manual_table argument does not have the correct columns. See documentation.")
  }
  
  #function begins here ----
  comp_notes = split(comp_table, row(comp_table[,1]))
  
  #initialise output
  classes = rep(NA,length(comp_notes))
  
  #loop over each note
  for(i in 1:length(comp_notes)){
    note = comp_notes[[i]]
    midpoint = (note$end + note$start)/2
    
    #take all notes from the manual_table coming from the same recording
    manual_table_notes = manual_table %>%
      dplyr::filter(sound.files == note$sound.files)
    
    #logical vector indicating match
    match_vec = data.table::between(midpoint,  manual_table_notes$start,  manual_table_notes$end)
    
    #save match note if it exists, otherwise record unclassified
    if(any(match_vec)){
      match_index = which(match_vec)
      classes[i] =  manual_table_notes$note_label[match_index]
    } else {
      classes[i] = "unclassified"
    }
    #end loop over all notes in comp_table
  }
  
  return(classes)
}