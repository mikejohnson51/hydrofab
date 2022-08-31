context("Test refactor wrapper")

test_that("IO methods", {
  
  testthat::skip("this test needs to be fixed")
  
  source(system.file("extdata", "walker_data.R", package = "hydrofab"))
  
  refactor <- refactor(flowpaths  = walker_flowline,
                       catchments = walker_catchment,
                       split_flines_meters = 2000,
                       collapse_flines_meters = 1,
                       collapse_flines_main_meters = 1,
                       facfdr = 
                         list.files(pattern = "extdata$", full.names = TRUE, 
                                    include.dirs = TRUE, recursive = TRUE))
  
  expect_true(nrow(refactor$flowpaths) == nrow(refactor$catchments), "failed wrapper")

})