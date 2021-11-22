test_that("note_match works",{
  
  #manual check
  
  #everything the same recording
  
  #in order (matched, unmatched, matched)
  comp_table = tibble::tibble(start = c(0.35, 0.5, 0.62), end = c(0.49, 0.59, 0.73), sound.files = "JS001.wav")
  manual_table = tibble::tibble(start = c(0.37, 0.6), end = c(0.45, 0.7), sound.files = "JS001.wav", 
                                note_label = c("Curve","Drop")
                                )
  output = note_match(comp_table, manual_table)
  ans = c("Curve", "unclassified","Drop")
  expect_equal(output,ans)
  
  #different recordings involved
  comp_table1 = tibble::tibble(start = c(0.35, 0.5, 0.62), end = c(0.49, 0.59, 0.73), sound.files = "JS001.wav")
  comp_table2 = tibble::tibble(start = c(0.6, 1.2, 1.6), end = c(0.85, 1.5, 2), sound.files = "JS002.wav")
  comp_table = rbind(comp_table1,comp_table2)
  
  manual_table1 = tibble::tibble(start = c(0.37, 0.6), end = c(0.45, 0.7), sound.files = "JS001.wav", 
                                note_label = c("Curve","Drop"))
  manual_table2 = tibble::tibble(start = c(0.58, 1.15), end = c(0.75, 1.45), sound.files = "JS002.wav", 
                                 note_label = c("Curve","Drop"))
  manual_table = rbind(manual_table1, manual_table2)
  
  output = note_match(comp_table, manual_table)
  ans = c("Curve", "unclassified","Drop", "Curve", "Drop", "unclassified")
  expect_equal(output,ans)
})