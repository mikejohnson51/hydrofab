context("Geometry Fixes")

test_that("Reconciled catchmetns can be fixed...", {
  div = read_sf(file.path(extdata, "gage_01013500.gpkg"), "raw-divides") 
  
  expect_false(length(st_cast(div$geom, "POLYGON")) == nrow(div))
  
  divides_new = catchment_geometry_doctor(div, "ID", keep = .9)
  
  expect_true(length(st_cast(divides_new$geom, "POLYGON")) == nrow(div))
  
})