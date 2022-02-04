context("Geometry Fixes")

test_that("Reconciled catchments can be fixed...", {
  
  source(system.file("extdata", "geometry_data.R", package = "hyRefactor"))
  
  expect_false(length(st_cast(st_geometry(test_divides), "POLYGON")) == nrow(test_divides))
  
  divides_new = clean_geometry(test_divides, "ID", keep = .9)

  expect_true(length(st_cast(st_geometry(divides_new), "POLYGON")) == nrow(test_divides))
  
})

test_that("Make sure 'out' passes when NULL...", {
  
  catchments = readRDS(list.files(pattern = "null_geom_catch.rds$", 
                                  full.names = TRUE, 
                                  recursive = TRUE))
  
  divides_new = clean_geometry(catchments, ID = "ID", keep = .9)
  
  expect_true(nrow(divides_new) == 6)
  
})