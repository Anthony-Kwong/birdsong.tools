test_that("gap_var_score function works",{
  unit_table = tibble::tibble(start = c(0.37, 0.6, 0.75, 0.95, 1.8, 2.5),
                              end = c(0.45, 0.7, 0.9, 1.4, 2, 3),
                              selec = c(1,2,3,1,2,3),
                              sound.files = c(rep("JS001.wav",6)),
                              note_label = c(rep("A",3), rep("B",3) ))
  gap_table = get_gaps(unit_table)
  
  #compute var scores for each transition type
  scores = NA
  counts = NA
  trans_types = unique(gap_table$transitions)
  for(i in 1:length(trans_types)){
    fil_data = dplyr::filter(gap_table, transitions == trans_types[i])
    scores[i] = get_var_score_dep(fil_data) 
    counts[i] = nrow(fil_data)
  }
  
  ans = 3/(5-2)*scores[1] + 2/(5-2)*scores[2]
  
  output = gap_var_score(gap_table)
  expect_equal(ans, output)
})