test_that("compute_oscores_dep2 works",{
  table1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
                          sound.files = "JS001.wav" , pos =c(1,2,3,4), note_label = "Curve")
  table2 = tibble::tibble(start = c(0.25, 0.65, 1.6, 2.4), end = c(0.45, 0.75, 2.1, 2.9), 
                          sound.files = "JS001.wav", pos =c(1,2,3,4), note_label = "Unclassifed")
  output = compute_oscores_dep2(table1 = table1, table2 = table2)
  
  #compute answer ----
  notelist = list(
    c(0.25,0.45),
    c(0.65,0.75),
    c(1.6,2.1),
    c(2.4,2.9)
    #start = c(0.25, 0.65, 1.6, 2.4), end = c(0.45, 0.75, 2.1, 2.9)
  )
  scores = list()
  for(i in 1:4){
    scores[[i]] = compute_oscores_dep(note = unlist(notelist[[i]]), table = table1)
  }
  score_tab = do.call(rbind, scores)
  final_scores = colSums(score_tab)
  names(final_scores) = c("sc1" ,"sc2" ,"sc3")
  expect_equal(output, final_scores)
})
