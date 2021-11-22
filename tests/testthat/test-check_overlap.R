test_that("check_overlap works",{
  #note2 contained in note1
  note1 = c(1,5)
  note2 = c(3,4)
  output = check_overlap(note1, note2)
  expect_equal(output,1)
  
  #note1 contained in note2
  note1=c(3,5)
  note2=c(2,10)
  output = check_overlap(note1, note2)
  expect_equal(output,2)
  
  #note1 left overlap with note2
  note1 = c(10,13)
  note2 = c(11,15)
  output = check_overlap(note1, note2)
  expect_equal(output, 2)
  
  #note1 right overlap with note2
  note1 = c(15,20)
  note2 = c(12,19)
  output = check_overlap(note1, note2)
  expect_equal(output, 4)
  
  #no overlap
  note1=c(2,7)
  note2=c(8,10)
  output = check_overlap(note1, note2)
  expect_equal(output,0)
  
})