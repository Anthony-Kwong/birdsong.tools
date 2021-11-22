#' note_compare function
#' 
#' Takes a note from one unit table and compares it with the corresponding note
#' found in another unit table. 
#'
#' @param note : A unit table tibble containing the original sound file, 
#' start/end times of the note and the note class. 
#' @param unit_table : A tibble of notes containing the original sound file, 
#' start/end times of the note and the note class.
#'
#' @return A tibble with the sound file, the square difference in the intervals, a 
#' logical indicating whether a note match was found and the note class. 
#' If no match it found, the difference is the square interval length of the input note. 
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate
#' @importFrom data.table between
#' @importFrom magrittr %>%
#' @export
#'
#' @examples 
#' note = tibble(start = 0.35, end = 0.49, sound.files = "JS001.wav", pnote_label = "Curve")
#' unit_table = tibble(start = c(0.35, 0.55), end = c(0.49, 0.7), sound.files = "JS001.wav")
#' note_compare(note, unit_table)

note_compare <- function(note, unit_table){
  
  #check inputs
  
  #check all the correct columns are present
  correct_cols = c("start", "end", "sound.files","note_label")
  if(sum(colnames(note) %in% correct_cols ) != length(correct_cols) ){
    stop("note argument does not have the correct columns. See documentation.")
  }
  
  correct_cols = c("start", "end", "sound.files")
  if(sum( colnames(unit_table) %in% correct_cols ) != length(correct_cols) ){
    stop("unit_table argument does not have the correct columns. See documentation.")
  }
  
  #function starts here ----
  
  #take name of soundfile which generated selected note
  recording = note$sound.files
  
  #take all the notes from the same recording
  candidate_notes = unit_table %>%
    dplyr::filter(sound.files == recording)
  
  note_midpoint = (note$end + note$start)/2
  
  #find the corresponding note
  candidate_notes = candidate_notes %>%
    dplyr::mutate(match = data.table::between(note_midpoint,
                                       candidate_notes$start,
                                       candidate_notes$end)
    )
  
  #compute difference between becky's note and corresponding matched note
  if(any(candidate_notes$match)){
    match = T
    matched_note = candidate_notes[which(candidate_notes$match),]
    diff = (note$start - matched_note$start)^2 +
      (note$end - matched_note$end)^2
  } else {
    diff = (note$end-note$start)^2
    match = F
  }
  
  result = tibble::tibble(
    sound_file = recording, 
    difference = diff,
    matched = match,
    note_label = note$note_label 
  )
  
  return(result)
}

