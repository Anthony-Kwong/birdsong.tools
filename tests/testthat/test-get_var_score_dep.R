test_gvs_dep <- function(gap_table, denom_var = T, min = 2){
  mean_gap = mean(gap_table$gap_dur)
  n = nrow(gap_table)
  
  if(n < min){
    return(0)
  }

  if(denom_var){
    v = sapply(gap_table$gap_dur, function(x){
      (x - mean_gap)^2/(n-1)
    })
  } else {
    v = sapply(gap_table$gap_dur, function(x){
      (x - mean_gap)^2/(n)
    })
  }
  
  sum_v = sum(v)
  uv = sqrt(sum_v)
  norm_s = uv/mean_gap  
  return(norm_s)
}

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
  ans = test_gvs_dep(gap_table)
  expect_equal(ans,output)  
    
})