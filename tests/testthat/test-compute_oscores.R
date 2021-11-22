test_that("compute_oscores",{
  #test1
  table1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
                          sound.files = c(rep("JS001.wav",2),rep("JS002.wav",2)) , pos =c(1,2,3,4), note_label = "Curve")
  table2 = tibble::tibble(start = c(0.25, 0.65, 1.6, 2.4), end = c(0.45, 0.75, 2.1, 2.9), 
                          sound.files = c(rep("JS001.wav",2),rep("JS002.wav",2)), pos =c(1,2,3,4), note_label = "Unclassifed")
  
  output = compute_oscores(table1,table2)
  
  #compute answer
  truth_tables = list(
    table1[1:2,],
    table1[3:4,]
  )
  comp_tables = list(
    table2[1:2,],
    table2[3:4,]
  )
  
  table_scores = list()
  for(i in 1:2){
    table_scores[[i]] = compute_oscores_dep2(truth_tables[[i]],comp_tables[[i]])
  }
  score_table = do.call(rbind, table_scores)
  ans = colSums(score_table)
  expect_equal(output,ans)
  
  #test2, recordings not in the same order
  table1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
                          sound.files = c(rep("JS001.wav",2),rep("JS002.wav",2)) , pos =c(1,2,3,4), note_label = "Curve")
  table2 = tibble::tibble(start = c(1.6, 2.4, 0.25, 0.65), end = c(2.1, 2.9, 0.45, 0.75), 
                          sound.files = c(rep("JS002.wav",2),rep("JS001.wav",2)), pos =c(1,2,3,4), note_label = "Unclassifed")
  
  output = compute_oscores(table1,table2)
  
  #compute answer
  table1 = table1[order(table1$sound.files),]
  table2 = table2[order(table2$sound.files),]
  
  truth_tables = list(
    table1[1:2,],
    table1[3:4,]
  )
  comp_tables = list(
    table2[1:2,],
    table2[3:4,]
  )
  
  table_scores = list()
  for(i in 1:2){
    table_scores[[i]] = compute_oscores_dep2(truth_tables[[i]],comp_tables[[i]])
  }
  score_table = do.call(rbind, table_scores)
  ans = colSums(score_table)
  expect_equal(output,ans)
})
