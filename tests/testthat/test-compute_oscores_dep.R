test_that("compute_oscores_dep works",{
  #test 1
  table1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
                          sound.files = "JS001.wav", pos =c(1,2,3,4), note_label = "Curve")
  
  note = c(0.48,0.69)
  output = compute_oscores_dep(table = table1, note = note)
  ans = overlap_score(note1 = c(table1[2,]$start,table1[2,]$end), note2 = note)
  expect_equal(as.numeric(output), ans)
  
  #test 2
  table2 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
                          sound.files = "JS001.wav", pos =c(1,2,3,4), note_label = "Curve")
  
  note = c(1.6,2.2)
  output = compute_oscores_dep(table = table2, note = note)
  ans = overlap_score(note1 = c(table2[3,]$start,table2[3,]$end), note2 = note)
  expect_equal(as.numeric(output), ans)
})