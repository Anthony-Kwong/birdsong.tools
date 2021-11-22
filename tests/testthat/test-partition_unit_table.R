test_that("partition_unit_table works",{
  #test1
  unit_table = tibble::tibble(start = c(0.37, 0.6, 0.8, 1.8), end = c(0.45, 0.7, 0.9, 2), pos = c(1,2,3,4),
                              sound.files = c( rep("JS001.wav",2), rep("JS002.wav",2) ), 
                              note_label = c("A","B","C","D"))
  
  output = partition_unit_table(unit_table)
  ans = list (
    tibble::tibble(start = c(0.37, 0.6), end = c(0.45, 0.7), pos = c(1,2),
                   sound.files = rep("JS001.wav",2), 
                   note_label = c("A","B")) ,
    
    tibble::tibble(start = c( 0.8, 1.8), end = c( 0.9, 2), pos = c(3,4),
                   sound.files = rep("JS002.wav",2) , 
                   note_label = c("C","D"))
  )
  
  expect_equal(output, ans)
  
  #test2
  unit_table = tibble::tibble(start = c(0.37, 0.6, 0.8, 1.8), end = c(0.45, 0.7, 0.9, 2), pos = c(1,2,3,4),
                              sound.files = "JS001.wav", 
                              note_label = c("A","B","C","D"))
  
  output = partition_unit_table(unit_table)
  expect_equal(output, list(unit_table))
  
})