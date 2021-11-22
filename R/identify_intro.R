#' identify_intro function
#' 
#' Takes a unit table and identifies the notes that are part of the introduction. 
#' The first note is always part of the introduction. Subsequent notes are included
#' into the introduction until there are at least 5 continguous notes which are 
#' <0.25s apart. 
#' 
#'
#' @param unit_table: A unit table tibble containing the original sound file, 
#' start/end times of the note,the note position and note_label. 
#'
#' @return The input unit table with an additional column indicating whether 
#' each note is in an introduction or not. 
#' @export
#'
#' @examples unit_table = tibble::tibble(start = c(0.37, 0.6, 0.75, 0.2, 1.8, 2.5), end = c(0.45, 0.7, 0.9, 0.4, 2, 3), pos = c(1,2,3,1,2,3),sound.files = c(rep("JS001.wav",3),rep("JS002.wav", 3)), note_label = "Curve")
#' identify_intro(intro_table)
identify_intro <- function(unit_table){
  
  #separate the unit table by recording
  song_tables = partition_unit_table(unit_table)
  
  #find intros for each song
  updated_song_tables = lapply(song_tables, function(table){
    intro = identify_intro_dep(table)
    table = cbind(table, intro)
    })
  
  res = do.call(rbind, updated_song_tables)
  return(res)
}

# numbers = c(1,2,3,5,7,8)
# 
# difference = diff(numbers) == 1
# ## [1]  TRUE  TRUE FALSE FALSE  TRUE
# 
# ## find alteast one consecutive TRUE
# any(tail(difference, -1) &
#       head(difference, -1))
