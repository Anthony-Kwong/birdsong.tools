test_that("get_gaps works",{
  unit_table = tibble::tibble(start = c(0.37, 0.6, 0.75, 0.2, 1.8, 2.5), 
                              end = c(0.45, 0.7, 0.9, 0.4, 2, 3), 
                              selec = c(1,2,3,1,2,3),
                              sound.files = c(rep("JS001.wav",3),rep("JS002.wav", 3)), 
                              note_label = rep(c("A","B","C"),2))
  output = get_gaps(unit_table)
  ans = tibble::tibble(
    sound.files = c(rep("JS001.wav",2), rep("JS002.wav",2)),
    gap_dur = c(0.6 - 0.45, 0.75 - 0.7, 1.8-  0.4, 2.5 - 2),
    selec = c(1,2,1,2),
    transitions = rep( c("A-B","B-C"), 2)
  )
  
  expect_equal(output, ans)
  
  #åexpect_equal(1,2)
})

#add transitions to get_gaps function

