test_that("add_metadata works",{
  data = tibble::tibble(Bird.ID = c("JS001", "JS002"),
                        a = c(1,2), b = c(3,4), c = c(5,6))
  metadata = tibble::tibble(Bird.ID = c("JS001", "JS002"),
                            x = c(1,2), y = c(3,4), z = c(5,6))
  
  output = add_metadata(data = data,metadata = metadata, cols = c(2,3))
  ans = cbind(data, x = metadata$x, y = metadata$y)
  
  expect_equal(output, ans)
})