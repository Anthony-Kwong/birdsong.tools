test_that("add_transitions works",{
  
  #test1 ----
  unit_table = tibble::tibble(start = c(0.37, 0.6, 0.2, 1.8), end = c(0.45, 0.7, 0.4, 2), selec = c(1,2,1,2),
                              sound.files = c("JS001.wav","JS001.wav","JS002.wav","JS002.wav")
                              ,note_label = c("A","B","C","D"))
  output = add_transitions(unit_table) 
  ans = tibble::tibble(start = c(0.37, 0.6, 0.2, 1.8), end = c(0.45, 0.7, 0.4, 2), selec = c(1,2,1,2),
                       sound.files = c("JS001.wav","JS001.wav","JS002.wav","JS002.wav")
                       ,note_label = c("A","B","C","D"),
                       transitions = c("A-B","end","C-D","end")) %>%
                       data.table::data.table()
  expect_equal(output,ans)
  
  #test2 ----
  unit_table = tibble::tibble(start = seq(6), end = seq(6)+0.5, selec = c(1,2,3,3,1,2),
                              sound.files = c(rep("JS001.wav", 3), rep("JS002.wav", 3))
                              ,note_label = c("A","B","C","D","E","F"))
  output = add_transitions(unit_table)
  ans = tibble::tibble(start = c(1,2,3,5,6,4), end = c(1,2,3,5,6,4) + 0.5, 
                       selec = c(seq(3),seq(3)) ,
                       sound.files = c(rep("JS001.wav", 3), rep("JS002.wav", 3))
                       ,note_label = c("A","B","C","E","F","D"),
                       transitions = c("A-B","B-C","end","E-F","F-D","end" )) %>%
                       data.table::data.table()
  expect_equal(output,ans)
})