test_compute_IOI <- function(unit_table){
  n = nrow(unit_table) - 1
  tk = c()
  starts = unit_table$start
  for(i in 1:n){
    tk[i] = starts[i+1] - starts[i]
  }
  tl = c(tk[2:n],NA)
  res = tibble::tibble(sound.files = "JS001.wav", t1 = tk, note1 = unit_table$note_label[1:n], 
                       t2 = tl, note2 = c(unit_table$note_label[2:n],NA), pos = seq(n))
  return(res)
}

test_that("compute_IOI function works",{
  #test 1
  unit_table = tibble::tibble(start = c(0.37, 0.6, 1.2, 1.9), end = c(0.45, 0.7, 1.4, 2),
                              pos = seq(1,4),sound.files = "JS001.wav" , note_label = rep(c("A","B"),2))
  
  #compute IOI bounds manually in loop
  t1 = c(0.6-0.37, 1.2 - 0.6, 1.9 - 1.2)
  t2 = c(t1[2:length(t1)], NA)
  ans = tibble::tibble(sound.files = "JS001.wav", t1, note1 = unit_table$note_label[1:length(t1)], 
                       t2, note2 = c(unit_table$note_label[2:length(t1)],NA), pos = seq(3))
  
  output = compute_IOI(unit_table)
  expect_equal(ans, output)
  expect_equal(ans, test_compute_IOI(unit_table))
  
  #test2
  unit_table = tibble::tibble(start = c(0.1, 1.5, 3, 5, 7), end = c(0.4, 1.6, 3.2, 5.5, 8.5),
                              pos = seq(1,5),sound.files = "JS001.wav" , note_label = c("A","B","C","D","A"))
  output = compute_IOI(unit_table)
  ans = test_compute_IOI(unit_table)
  expect_equal(output, ans)
})
