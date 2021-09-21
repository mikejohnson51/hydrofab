context("Geometry Fixes")

test_that("Reconciled catchmetns can be fixed...", {
  
  source(system.file("extdata", "geometry_data.R", package = "hyRefactor"))
  
  expect_false(nrow(st_cast(test_divides, "POLYGON")) == nrow(test_divides))
  
  divides_new = clean_geometry(test_divides, "ID", keep = .9)

  expect_true(nrow(st_cast(divides_new, "POLYGON")) == nrow(test_divides))
  
})