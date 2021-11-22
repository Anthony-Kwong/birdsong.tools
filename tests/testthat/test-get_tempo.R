test_that("get_tempo works",{
  unit_table = tibble::tibble(start = c(0.35, 0.6, 0.2, 1.8), 
                              end = c(0.45, 0.7, 0.4, 2), pos = c(1,2,1,2),
                              sound.files = c("JS001.wav","JS001.wav","JS002.wav","JS002.wav") , 
                              note_label = "Curve")
  output = get_tempo(unit_table)
  ans = tibble::tibble(sound.files = c("JS001.wav","JS002.wav"),
                       tempo = c( 2/(0.7 - 0.35), 2/(2 - 0.2)))
  
  expect_equal(output, ans)
})