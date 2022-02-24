test_that("gap_score function works",{
  unit_table = tibble::tibble(start = c(0.37, 0.6, 0.75, 1.2, 1.8, 2.5), 
                              end = c(0.45, 0.7, 0.9, 1.4, 2, 3), 
                              selec = seq(6),
                              sound.files = "JS001.wav", 
                              note_label = rep(c("A","B","C"),2))
  gap_table = get_gaps(unit_table)
  
  pop_means = tibble::tibble(transitions = c("A-B", "B-C", "C-A"), 
                             mean_gap = c(0.2, 0.3, 0.4))
  
  AB = log (mean( c(0.15 ,0.4) )/0.2 )
  BC = log( mean( c(0.05 , 0.5) )/0.3 )
  CA = log( 0.3/0.4 )
  
  ans= mean( c(AB , BC , CA) )
  output = gap_score(gap_table, pop_means)
  expect_equal(ans, output)
})