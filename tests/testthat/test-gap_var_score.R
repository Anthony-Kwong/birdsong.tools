test_fct <- function(gap_table, ID){
  bird_data = dplyr::filter(gap_table, Bird.ID == "JS001")
  
  trans = unique(bird_data$transitions)
  
  #compute Cjb for each transition
  Cjb = c()
  for(i in 1:length(trans)){
    d = dplyr::filter(bird_data, transitions == trans[i])
    Cjb[i] = get_var_score_dep(d)
  }
  
  #compute Cj
  #nbirds = unique(gap_table$Bird.ID)
  Cj = c()
  for(j in 1:length(trans)){
    Cj[j] = get_var_score_dep2(trans[j], gap_table)
  }
  
  #Compute Z
  Z = c()
  for(k in 1:length(trans)){
    Z[k] = log(Cjb[k]/Cj[k])
  }
  
  #Compute G
  G = c()
  for(p in 1:length(trans)){
    d2 = dplyr::filter(bird_data, transitions == trans[p])
    njb = nrow(d2)
    G[p] = (njb - 1)*Z[p]
  }
  
  #normalize G
  Nb = nrow(bird_data)
  Kb = length(trans)
  
  # test = tibble::tibble(Cjb = Cjb, Cj = Cj, Z = Z, G)
  # print(test)
  
  Gb = sum(G)/(Nb - Kb)
  return(Gb)
}

test_that("gap_var_score function works",{
  tp = runif(10, 0, 10)
  
  unit_table1 = tibble::tibble(start = c(0.37, 0.6, 0.75, 0.95, 1.8, 2.5),
                               end = c(0.45, 0.7, 0.9, 1.4, 2, 3),
                               selec = seq(6),
                               sound.files = c(rep("JS001.wav",6)),
                               note_label = c("A","B","A","B","A","B"))
  unit_table2 = tibble::tibble(start = c(0.30, 0.5, 0.75, 0.95, 1.8, 2.5),
                               end = c(0.45, 0.7, 0.9, 1.4, 2, 3),
                               selec = seq(6),
                               sound.files = c(rep("JS002.wav",6)),
                               note_label = c("A","B","A","B","A","B"))
  gap_table = get_gaps(rbind(unit_table1,unit_table2))
  gap_table = dplyr::mutate(gap_table,Bird.ID = c(rep("JS001",5), rep("JS002",5) ))
  
  output = gap_var_score(gap_table, ID = "JS001")
  test = test_fct(gap_table, ID = "JS001")
  expect_equal(output, test)
})