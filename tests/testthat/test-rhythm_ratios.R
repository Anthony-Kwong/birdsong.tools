test_that("rhythm_ratios work",{
  #test 1
  unit_table = tibble::tibble(start = c(0.37, 0.6, 1.2, 1.9), end = c(0.45, 0.7, 1.4, 2),
                              pos = seq(1,4),sound.files = "JS001.wav" , note_label = "Curve")
  output = rhythm_ratios(unit_table)
  
  #manually compute answer
  onset = c(0.6-0.37, 1.2 - 0.6, 1.9-1.2)
  ans = c(onset[1]/(onset[2]+onset[1]), onset[2]/(onset[3]+onset[2]))
  expect_equal(ans, output)
  
  #test 2
  unit_table = tibble::tibble(start = c(0.1, 1.5, 3, 5), end = c(0.4, 1.6, 3.2, 5.5),
                              pos = seq(1,4),sound.files = "JS001.wav" , note_label = "Curve")
  output = rhythm_ratios(unit_table)
  
  #manually compute answer
  onset = c(1.5 - 0.1, 3 - 1.5, 5-3)
  ans = c(onset[1]/(onset[2]+onset[1]), onset[2]/(onset[3]+onset[2]))
  expect_equal(ans, output)
  
  #test 3
  unit_table = tibble::tibble(start = c(0.1, 2, 2.1, 6, 6.1), end = c(0.4, 2.05, 2.2, 6.05, 6.6),
                              pos = seq(1,5),sound.files = "JS001.wav" , note_label = "Curve")
  output = rhythm_ratios(unit_table)
  
  #manually compute answer
  onset = c(2 - 0.1, 2.1-2, 6-2.1, 6.1-6)
  ans = c(onset[1]/(onset[2]+onset[1]), onset[2]/(onset[3]+onset[2]), onset[3]/(onset[4]+onset[3]))
  expect_equal(ans, output)
})