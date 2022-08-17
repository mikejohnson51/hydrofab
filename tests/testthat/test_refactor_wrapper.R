context("Test refactor wrapper")

test_that("IO methods", {
  
  source(system.file("extdata", "walker_data.R", package = "hyRefactor"))
  
  refactor <- refactor(flowpaths  = walker_flowline,
                       catchments = walker_catchment,
                       split_flines_meters = 2000,
                       collapse_flines_meters = 1,
                       collapse_flines_main_meters = 1,
                       facfdr = '../../inst/extdata/')
  
  expect_true(nrow(refactor$flowpaths) == nrow(refactor$catchments), "failed wrapper")

})