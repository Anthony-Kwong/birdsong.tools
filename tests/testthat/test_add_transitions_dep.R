test_that("add_transitions_dep",{
  unit_table = tibble::tibble(start = c(0.37, 0.6, 0.8, 1.8), end = c(0.45, 0.7, 0.9, 2), selec = c(1,2,3,4),
                              sound.files ="JS001.wav" , note_label = c("A","B","C","D"))
  ans = tibble::tibble(start = c(0.37, 0.6, 0.8, 1.8), end = c(0.45, 0.7, 0.9, 2), selec = c(1,2,3,4),
                       sound.files ="JS001.wav" , note_label = c("A","B","C","D"),
                       transitions = c("A-B","B-C","C-D","end"))
  output = add_transitions_dep(unit_table) %>% tibble::tibble()
  expect_equal(ans, output)
})