#' rhythm ratios function
#' 
#' Computes the rhythm ratios using the a table of IOI (output of compute_IOI). Onset intervals are defined as the time between the start of one note
#' and the start of the subsequent note. Rhythm ratios are the ratios of adjacent onset intervals. 
#' https://doi-org.manchester.idm.oclc.org/10.1016/j.cub.2021.09.032
#'
#' @param ioi: An ioi tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label.  
#' 
#' @importFrom tibble tibble
#'
#' @return: A vector of onset intervals 
#' @export
#'
#' @examples unit_table = tibble::tibble(start = c(0.37, 0.6, 1.2, 1.8), end = c(0.45, 0.7, 1.4, 2), pos = seq(1,4),sound.files = "JS001.wav" , note_label = "Curve")
#' ioi = compute_IOI(unit_table)
#' onset_intervals(ioi)
rhythm_ratios <- function(ioi){
  #compute rhythm ratios
  r = ioi$t1/(ioi$t1+ioi$t2)
  #omit NAs since the last ratio can't be computed
  res = as.numeric(na.omit(r))
  return(res)
}