test_that("get_var_score_dep works",{
  #check one row case, should return 0
  gap_table = tibble::tibble(
    sound.files = "JS001.wav",
    gap_dur = 0.10,
    selec = 1,
    transitions = "A-A"
  )
  output = get_var_score_dep(gap_table)
  suppressMessages(
    expect_equal(0, output)
  )
  
  #check computation
  gaps = c(0.1,0.2,0.3)
  gap_table = tibble::tibble(
    sound.files = "JS001.wav",
    gap_dur = gaps,
    selec = c(1,2,3),
    transitions = "A-A"
  )
  output = get_var_score_dep(gap_table)
  ans = sd( gaps/mean(gaps) )^2
  expect_equal(ans,output)  
    
})