test_that("rhythm_ratios work",{
  unit_table = tibble::tibble(start = c(0.37, 0.6, 1.2, 1.9), end = c(0.45, 0.7, 1.4, 2),
                              pos = seq(1,4),sound.files = "JS001.wav" , note_label = "Curve")
  output = rhythm_ratios(unit_table)
  
  #manually compute answer
  onset = c(0.6-0.37, 1.2 - 0.6, 1.9-1.2)
  ans = c(onset[1]/(onset[2]+onset[1]), onset[2]/(onset[3]+onset[2]))
  expect_equal(ans, output)
})