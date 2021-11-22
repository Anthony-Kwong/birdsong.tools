test_that("note_compare works",{
  
  #test with one sound file only----
  
  #match found test, 1st note match
  note = tibble::tibble(start = 0.35, end = 0.49, sound.files = "JS001.wav",
                        note_label = "Curve")
  
  unit_table = tibble::tibble(start = c(0.30, 0.55, 1.5), end = c(0.51, 0.7, 2.2), 
                      sound.files = "JS001.wav")
  ans = tibble::tibble(sound_file = "JS001.wav",
                       difference = (0.35-0.3)^2 + (0.49-0.51)^2,
                       matched = T,
                       note_label = "Curve")
  
  output = note_compare(note, unit_table)
  expect_equal(ans,output)
  
  #no match test
  note = tibble::tibble(start = 0.1, end = 0.2, sound.files = "JS001.wav",
                        note_label = "Curve")
  
  unit_table = tibble::tibble(start = c(0.30, 0.55, 1.5), end = c(0.51, 0.7, 2.2), 
                              sound.files = "JS001.wav")
  ans = tibble::tibble(sound_file = "JS001.wav",
                       difference = (0.2-0.1)^2,
                       matched = F,
                       note_label = "Curve")
  
  output = note_compare(note, unit_table)
  expect_equal(ans,output)
  
  #match found test, 3rd note match
  
  note = tibble::tibble(start = 1.4, end = 2.3, sound.files = "JS001.wav",
                        note_label = "Curve")
  
  unit_table = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
                              sound.files = "JS001.wav")
  ans = tibble::tibble(sound_file = "JS001.wav", 
                       difference = (1.4-1.5)^2 + (2.3-2.2)^2,
                       matched = T,
                       note_label = "Curve")
  
  output = note_compare(note, unit_table)
  expect_equal(ans,output)
  
  #multiple sound files test
  
  note = tibble::tibble(start = 0.6, end = 0.7, sound.files = "JS002.wav", pos = 1,
                        note_label = "Curve")
  
  unit_table1 = tibble::tibble(start = c(0.30, 0.55, 1.5, 2.5), end = c(0.51, 0.7, 2.2, 3), 
                              sound.files = "JS001.wav")
  unit_table2 = tibble::tibble(start = c(0.2, 0.4, 0.55, 2.5), end = c(0.35, 0.5, 0.8, 3), 
                              sound.files = "JS002.wav")
  unit_table = rbind(unit_table1, unit_table2)
  
  ans = tibble::tibble(sound_file = "JS002.wav", 
                       difference = (0.6-0.55)^2 + (0.7-0.8)^2,
                       matched = T,
                       note_label = "Curve")
  
  output = note_compare(note, unit_table)
  expect_equal(ans,output)
})