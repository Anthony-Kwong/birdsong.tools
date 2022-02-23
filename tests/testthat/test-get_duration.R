test_that("get_duration works",{
  unit_table = tibble::tibble(start = c(0.37, 0.6, 0.75, 1.2, 1.8, 2.5),
                              end = c(0.45, 0.7, 0.9, 1.4, 2, 3), 
                              sound.files = "JS001.wav",
                              select = seq(6),
                              note_label = "Curve")
  #end of last note minus start of first note
  ans = 3-0.37
  output = get_duration(unit_table)
  expect_equal(output, ans)
})