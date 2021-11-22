#test function for compare_excision

compare_excision2 <- function(table1,table2){
  
  n_table1 = nrow(table1)
  diffs1 = lapply(1:n_table1, c)
  #loop through all elements of table1 and compare with table2
  for(i in 1:n_table1){
    note = table1[i,]
    diffs1[[i]] = note_compare(note, table2)
  }
  
  n_table2 = nrow(table2)
  diffs2 = lapply(1:n_table2, c)
  #loop through all elements of table2 and compare with table1
  for(i in 1:n_table2){
    note = table2[i,]
    diffs2[[i]] = note_compare(note, table1)
  }
  
  diff = c(diffs1,diffs2) 
  res = plyr::rbind.fill(diff)
  return(res)
}

test_that("compare_excision works",{
  
  #construct 1st unit table
  unit_table_p1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
                               sound.files = "JS001.wav", pos =c(1,2,3,4), note_label = "Curve")
  unit_table_p2 = tibble::tibble(start = c(0.2, 0.4, 0.55, 2.5), end = c(0.35, 0.5, 0.8, 3), 
                               sound.files = "JS002.wav", pos =c(1,2,3,4), note_label = "Curve2")
  unit_table1 = rbind(unit_table_p1,unit_table_p2)
  
  #construct 2nd unit table
  unit_table2_p1 = tibble::tibble(start = c(0.25, 0.65, 2), end = c(0.45, 0.75, 2.7), 
                                 sound.files = "JS001.wav", pos =c(1,2,3), note_label = "Unclassifed")
  unit_table2_p2 = tibble::tibble(start = c(0.15, 0.50, 2.75), end = c(0.30, 0.75, 3.2), 
                                 sound.files = "JS002.wav", pos =c(1,2,3),note_label = "Unclassifed")
  unit_table2 = rbind(unit_table2_p1,unit_table2_p2)
  
  
  output = compare_excision(unit_table1,unit_table2)
  ans = compare_excision2(unit_table1,unit_table2)
  
  expect_equal(output,ans)
  
})

# becky = read_csv("~/work/PhD/Data/Files for Anthony/Files for Anthony/JavaSparrow_UnitTable.csv")
# 
# anthony = read_csv("~/work/PhD/Data/note_excision/datasets/measurements.csv")

# #data cleaning
# 
# anthony = anthony %>%
#   dplyr::rename(sound.files = filename,
#                 start = interval_start, 
#                 end = interval_end) %>%
#   dplyr::filter(interval_label == "sounding")
# 
# library(tidyverse)
# 
# #data manipulation
# becky = becky %>%
#   mutate(midpoint = (start + end)*0.5 )
# 
# #loop through individual notes
# becky_note = becky[1,]
# 
# recording = becky_note$sound.files
# 
# #take all the notes from the same recording
# candidate_notes = anthony %>%
#   dplyr::filter(sound.files == recording)
# 
# #find the corresponding note
# candidate_notes = candidate_notes %>%
#   mutate(match = data.table::between(becky_note$midpoint,
#                                      candidate_notes$interval_start,
#                                      candidate_notes$interval_end)
#          )
# 
# #compute difference between becky's note and corresponding matched note
# if(any(candidate_notes$match)){
#   match = T
#   matched_note = candidate_notes[which(candidate_notes$match),]
#   diff = (becky_note$start - matched_note$interval_start)^2 +
#     (becky_note$end - matched_note$interval_end)^2
# } else {
#   diff = NA
#   match = F
# }
# 
# tibble::tibble(
#   sound_file = recording, 
#   becky_note_pos = becky_note$selec,
#   anthony_note_pos = 1 , #change later
#   difference = diff,
#   matched = match
# )
