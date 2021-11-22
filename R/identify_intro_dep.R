#' identify_intro_dep
#' 
#' Dependency for identify_intro. Takes a unit table and assigns notes
#' to the introduction. The notes from the input are all from the same
#' recording. 
#'
#' @param unit_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label. Notes are 
#' all from the same recording.  
#'
#' @return A logical vector indicating
#' whether each note has been assigned to the introduction. 
#' @export
#'
#' @examples unit_table = tibble::tibble(start = c(0.2, 0.6, 0.75, 1.1, 1.8, 2.5, 4, 5, 6, 7, 8),
#'end = c(0.25, 0.7, 0.9, 1.6, 1.9, 3, 4.2, 5.5, 6.8, 7.6, 8.4), selec = seq(11),sound.files = "JS001.wav",
#'note_label = "Wave")
#'identify_intro_dep(unit_table)
#'
identify_intro_dep <- function(unit_table){
  
  #compute the gaps between all the notes
  gaps = get_gaps(unit_table)
  
  #identify notes which are <0.25s close
  close_notes = gaps$gap_dur < 0.25
  #compute run lengths 
  x = rle(close_notes)
  candidate_starts = 
  #compute the number of gaps for each section where the gaps are <0.25
  short_runlen = x$lengths[ which(x$values==T)]
  #intro stops at the first section with 5 notes (4 gaps) that are <0.25 apart
  
  
  # #identify notes which are <0.25s close
  # close_notes = gaps$gap_dur < 0.25
  # #find the first note of intro
  # start_index = which(close_notes)[1]
  # 
  # #find run length of intro
  # x = rle(close_notes)
  # #find the run length of the first T
  # run_len = x$lengths[which(x$values)[1]]
  # 
  # if(run_len > 5){
  #   intro_len = 5
  # } else {
  #   intro_len = run_len
  # }
  # 
  # end_index = start_index + intro_len - 1
  # 
  # #initialise intro vec
  # intro = rep(F,nrow(unit_table))
  # intro[start_index:end_index] = T
  # 
  return(intro)
}