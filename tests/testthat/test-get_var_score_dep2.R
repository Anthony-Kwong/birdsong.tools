test_fun <- function(transition, gap_table){
  data1 = gap_table %>%
    dplyr::filter(transitions == transition)
  
  birds = unique(gap_table$sound.files)
  top = c()
  bot = c()
  for(i in 1:length(birds)){
    ind_data =  data1 %>%
      dplyr::filter(sound.files == birds[i])
    Cjb = get_var_score_dep(ind_data)
    njb = nrow(ind_data)
    top[i] = Cjb*(njb-1)
    bot[i] = njb-1
  }
  
  Cj = sum(top)/sum(bot)
  return(Cj)
}

test_that("gap_var_score_dep2 function works",{
  unit_table1 = tibble::tibble(start = c(0.37, 0.6, 0.75, 0.95, 1.8, 2.5),
                              end = c(0.45, 0.7, 0.9, 1.4, 2, 3),
                              selec = seq(6),
                              sound.files = c(rep("JS001.wav",6)),
                              note_label = c(rep("A",3), rep("B",3) ))
  unit_table2 = tibble::tibble(start = c(0.30, 0.5, 0.75, 0.95, 1.8, 2.5),
                              end = c(0.45, 0.7, 0.9, 1.4, 2, 3),
                              selec = seq(6),
                              sound.files = c(rep("JS002.wav",6)),
                              note_label = c(rep("A",2), rep("B",4) ))
  gap_table = get_gaps(rbind(unit_table1,unit_table2))
  
  transition = "B-B"
  output = get_var_score_dep2(transition, gap_table)
  test_says = test_fun(transition, gap_table)
  
  expect_equal(output, test_says)
})