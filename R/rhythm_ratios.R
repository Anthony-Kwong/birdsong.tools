#' rhythm ratios function
#' 
#' Computes the rhythm ratios using a unit table. Onset intervals are defined as the time between the start of one note
#' and the start of the subsequent note. Rhythm ratios are the ratios of adjacent onset intervals. 
#' https://doi-org.manchester.idm.oclc.org/10.1016/j.cub.2021.09.032
#'
#' @param unit_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label.  
#'
#' @return: A vector of onset intervals 
#' @export
#'
#' @examples unit_table = tibble::tibble(start = c(0.37, 0.6, 1.2, 1.8), end = c(0.45, 0.7, 1.4, 2), pos = seq(1,4),sound.files = "JS001.wav" , note_label = "Curve")
#' onset_intervals(unit_table)
rhythm_ratios <- function(unit_table){
  start_times = unit_table$start
  onset = diff(start_times)
  #compute ratios
  r = sapply(seq(length(onset)-1), function(x){
    r = onset[x]/(onset[x] + onset[x+1] )
  })
  return(r)
  #current implementation does not compute the last ratio
}