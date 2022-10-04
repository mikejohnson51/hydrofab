context("Test refactor wrapper")

test_that("IO methods", {
  
  source(system.file("extdata", "walker_data.R", package = "hydrofab"))
  
  temp_dir <- file.path(tempdir(), "fdrfac")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  
  file.copy(file.path(extdata, "walker_fdr.tif"), file.path(temp_dir, "18b_fdr.tif"))
  file.copy(file.path(extdata, "walker_fac.tif"), file.path(temp_dir, "18b_fac.tif"))
  
  refactor <- refactor(flowpaths  = walker_flowline,
                       catchments = walker_catchment,
                       split_flines_meters = 2000,
                       collapse_flines_meters = 1,
                       collapse_flines_main_meters = 1,
                       facfdr = temp_dir)
  
  expect_true(nrow(refactor$flowpaths) == nrow(refactor$catchments), "failed wrapper")

})