#test function to check scramble_within_groups
compare_data_frames <- function(df1, df2) {
  df1_rows = split(df1, seq(nrow(df1)))
  df2_rows = split(df2, seq(nrow(df2))) 
  
  #check every row in df2 is also present in df1
  all_rows_found <- all(sapply(df2_rows, function(x) {
    any(sapply(df1_rows, function(y) {
      all(x==y)
    }))
  }))  
  return(all_rows_found)
}

test_that("scramble_within_groups function works",{
  #check the test function works
  df1 <- data.frame(
    X1 = c(1, 2, 3),
    X2 = c("A", "B", "C")
  )
  
  df2 <- data.frame(
    X1 = c(2, 3, 4),
    X2 = c("B", "C", "D")
  )
  
  a = compare_data_frames(df1, df2)
  b = compare_data_frames(df1, df1)
  c = compare_data_frames(df2, df2)
  
  expect_equal(a, F)
  expect_equal(b, T)
  expect_equal(c, T)
  
  #We will test that the groups in each row have not changed, but are otherwise different
  my_data <- data.frame(
    Col1 = c(1, 2, 3, 4, 5, 6),
    Col2 = c("A", "B", "A", "B", "C", "C")
  )
  
  res = scramble_within_groups(my_data,group_index = 2)
  #check classes are in the same order
  expect_equal(my_data$Col2, res$Col2)
  #check data column is different
  expect_equal(all(my_data$Col1 == res$Col1), F)
  #check the rows in output are all present in input
  expect_equal(compare_data_frames(my_data, res), T)
  
  
  #test 2 ----
  n = 5
  x = c(rep("A",n), rep("B",n), rep("C",n))
  y = sample(x, replace = F)
  
  my_data = data.frame(
    Col1 = seq(length(y)),
    Col2 = y
  )
  
  res = scramble_within_groups(my_data,group_index = 2)
  #check classes are in the same order
  expect_equal(my_data$Col2, res$Col2)
  #check data column is different
  expect_equal(all(my_data$Col1 == res$Col1), F)
  #check the rows in output are all present in input
  expect_equal(compare_data_frames(my_data, res), T)
})
