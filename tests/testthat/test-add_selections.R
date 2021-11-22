test_that("add_transitions works",{
  unit_table = tibble::tibble(sound.files = c( rep("JS001.wav",3), rep("JS002.wav",3)),
                              start = seq(from = 1, to = 3.5, by = 0.5),
                              end = seq(from = 1, to = 3.5, by = 0.5) + 0.5)
  
  output = add_selections(unit_table) 
  ans = tibble::tibble(sound.files = c( rep("JS001.wav",3), rep("JS002.wav",3)),
                       start = seq(from = 1, to = 3.5, by = 0.5),
                       end = seq(from = 1, to = 3.5, by = 0.5) + 0.5,
                       sel = c(seq(3),seq(3))) %>% 
                       data.table::as.data.table()
  
  expect_equal(output, ans)
})