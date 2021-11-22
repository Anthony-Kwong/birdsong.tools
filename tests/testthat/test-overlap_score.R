test_that("overlap_score works",{
  #note1 left of note 2, overlap
  note1 = c(1,6)
  note2 = c(4,9)
  
  output = overlap_score(note1,note2)
  x = 6-4
  a = 9-1
  y = 6-1
  z= 9-4
  ans = c(x/y, x/a, x^2/(y*z) )
  expect_equal(output,ans)
  
  #note2 right of note1, overlap
  note1 = c(7,10)
  note2 = c(5,8)
  output = overlap_score(note1,note2)
  x = 8-7
  a = 10-5
  y=10-7
  z=8-5
  ans = c(x/y, x/a, x^2/(y*z) )
  expect_equal(output,ans)
  
  #note1 all in note2
  note1= c(4,6)
  note2= c(3,8)
  output = overlap_score(note1,note2)
  x=6-4
  a=8-3
  y=6-4
  z=8-3
  ans = c(x/y, x/a, x^2/(y*z) )
  expect_equal(output,ans)
  
  #note2 all in note1
  note1=c(1,10)
  note2=c(5,7)
  output = overlap_score(note1,note2)
  x = 7-5
  a=10-1
  y=10-1
  z=7-5
  
  ans = c(x/y, x/a, x^2/(y*z) )
  expect_equal(output,ans)
  
  #no overlap
  note1=c(1,3)
  note2=c(6,9)
  output = overlap_score(note1,note2)
  ans=c(0,0,0)
  expect_equal(output,ans)
})