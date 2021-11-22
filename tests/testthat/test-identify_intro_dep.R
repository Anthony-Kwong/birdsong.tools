test_that("identify_intro_dep works",{

   unit_table = tibble::tibble(start = c(0.2, 0.6, 0.75, 1.1, 1.8, 2, 2.4,2.6),
                              end = c(0.25, 0.7, 0.9, 1.6, 1.9, 2.3, 2.5,2.7),
                              selec = seq(8),sound.files = "JS001.wav",
                              note_label = "Wave")
  
  output = identify_intro_dep(unit_table)
  ans = c(T,T,F,F,F,F,F,F)
  expect_equal(ans,output)
  
  #6 note intro, cut off at 5
  unit_table = tibble::tibble(start = c(0.21, 0.6, 0.75, 1.1, 1.8, 2, 3.1,4.3),
                              end = c(0.25, 0.7, 0.9, 1.7, 1.9, 3, 4.2,4.8),
                              selec = seq(8),sound.files = "JS001.wav",
                              note_label = "Curve")
  
  output = identify_intro_dep(unit_table)
  ans = c(F,T,T,T,T,T,F,F)
  expect_equal(ans,output)
})