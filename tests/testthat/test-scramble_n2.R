test_scramble_n2 <- function(ioi, index){
  #test function to ensure scramble_n2 is permuting rows properly
  data_slice = ioi[,4:6]
  #scramble the rows according to index argument
  perm_set = data_slice[index,]
  #rm last NA entry because we cannot compute r for last note
  data = na.omit(ioi)[,1:3]
  res = tibble::tibble(data, perm_set)
  return(res)
}

test_that("scramble_n2 works",{
  #test 1
  unit_table = tibble::tibble(start = c(0.37, 0.6, 1.2, 1.9, 2.3, 5, 6, 7), end = c(0.45, 0.7, 1.4, 2, 2.9, 5.5, 6.2, 7.5),
                              pos = seq(8),sound.files = "JS001.wav" , note_label = c(rep("A",4), rep("B",4)))
  ioi = compute_IOI(unit_table)
  output = scramble_n2(ioi)
  ans = test_scramble_n2(ioi, index = output[[2]])
  expect_equal(tibble::tibble(output[[1]]), ans)
  
  #test 2
  unit_table = tibble::tibble(start = c(0.37, 0.6, 1.2, 1.9, 2.3, 5, 6, 7), end = c(0.45, 0.7, 1.4, 2, 2.9, 5.5, 6.2, 7.5),
                              pos = seq(8),sound.files = "JS001.wav" , note_label = c(rep(c("A","B"), 4)))
  ioi = compute_IOI(unit_table)
  output = scramble_n2(ioi)
  ans = test_scramble_n2(ioi, index = output[[2]])
  expect_equal(tibble::tibble(output[[1]]), ans)
})