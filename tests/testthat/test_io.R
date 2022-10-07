context("hydrofabric I/O")

Sys.setenv(TURN_OFF_SYS_MAPSHAPER = "YUP")

test_that("gpkg I/O", {
  
  # Data Path
  gpkg = system.file("extdata", "gauge_01073000.gpkg", package = "hydrofab")
  
  # Execute Function w/ Default
  data = read_hydrofabric(gpkg, verbose = FALSE)
  
  # Were both flowpaths and divides read in?
  expect_true(length(read_hydrofabric(gpkg, verbose = FALSE)) == 2)

  # Read just divides (by name)...
  divides = read_hydrofabric(gpkg, 
                             catchments = "divides",
                             realization = "catchments", 
                             verbose = FALSE)
  
  # Do the divides come in as 5070 and equal to the full read?
  expect_true(st_crs(divides$catchments)$epsg == 5070)
  expect_equal(nrow(data$catchments), nrow(divides$catchments))
  
  # Read just flowlines (by name), check number and force new CRS
  fps = read_hydrofabric(gpkg, 
                         flowpaths = "flowpaths",
                         realization = "flowpaths", 
                         crs = 4326, verbose = FALSE)
  
  expect_true(st_crs(fps$flowpaths)$epsg == 4326)
  expect_equal(nrow(data$flowpaths), nrow(fps$flowpaths))

  
  data2 = read_hydrofabric(catchments  = divides$catchments, flowpaths = fps$flowpaths)
  expect_true(length(data2) == 2)
  
  outfile = tempfile(fileext = ".gpkg")
  o = write_hydrofabric(network_list = data2, outfile, catchment_name = 'catchments', verbose = FALSE)
  
  expect_true(layer_exists(o, "flowpaths"))
  expect_true(layer_exists(o, "catchments"))
  expect_false(layer_exists(o, "dogs"))
  expect_false(layer_exists("dogs", "flowpaths"))
  
  expect_true(length(read_hydrofabric(o, verbose = FALSE)) == 2)
  
  expect_error(sb_id("refactor"), NA)
  expect_error(sb_id("reference"), NA)
  expect_error(sb_id("uniform"), NA)
  expect_error(sb_id("minimal"), NA)
  expect_error(sb_id("dogs"))

})