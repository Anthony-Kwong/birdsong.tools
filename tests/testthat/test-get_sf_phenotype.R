test_that("get_sf_phenotype works",{
  #test1
  metadata = tibble::tibble(Bird.ID = c("JS01","JS02") , Social.Father = c("JS02",NA))
  phenotype_table = tibble::tibble(Bird.ID = c("JS01","JS02"), trait = c(2,3))
  output = get_sf_phenotype(metadata, phenotype_table,2)
  expect_equal(output, tibble::tibble(phenotype_table, sf_phenotype = c(3,NA)))
  
  #test2
  metadata = tibble::tibble(Bird.ID = c("JS01","JS02","JS03","JS04") ,
                            Social.Father = c(NA,"JS01",NA,"JS03"))
  phenotype_table = tibble::tibble(Bird.ID = c("JS01","JS02","JS03","JS04"),
                                   trait = c(10,20,30,40))
  output = get_sf_phenotype(metadata, phenotype_table,2)
  expect_equal(output, tibble::tibble(phenotype_table, sf_phenotype = c(NA,10,NA,30)))
  
})