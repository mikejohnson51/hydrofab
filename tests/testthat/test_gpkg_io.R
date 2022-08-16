context("Test hydrofabrci I/O methods")

test_that("IO methods", {
  
  gpkg = "data/io.gpkg"

  out =  read_hydrofabric(gpkg = gpkg)
  
  expect_equal(  st_crs(out$flowpaths)$epsg, st_crs(out$catchments)$epsg)
  
  out2 =  read_hydrofabric(gpkg = gpkg, catchments = "divides", flowpaths = "flowpaths")
  
  expect_identical(out$flowpaths, out2$flowpaths)
  
  out3 = read_hydrofabric(catchments = out$catchments, flowpaths = out$flowpaths)
  
  out4 = read_hydrofabric(catchments = out$catchments, flowpaths = out$flowpaths, crs = 5070)
  
  out5 = read_hydrofabric(catchments = NULL, flowpaths = out$flowpaths, crs = 5070)
  out6 = read_hydrofabric(catchments = out$catchments, flowpaths = NULL, crs = 5070)
  
  expect_equal(length(out5), 1)
  expect_equal(length(out6), 1)
    
  expect_true(st_crs(out3$flowpaths)$epsg != st_crs(out4$flowpaths)$epsg)
  expect_equal(st_crs(out4$flowpaths)$epsg, 5070)

})
