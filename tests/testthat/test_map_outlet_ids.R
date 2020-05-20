context("map_outlet_ids")

test_that("map_outlet_ids works", {
  test_data <- readRDS(list.files(pattern = "map_outlets.rds", full.names = TRUE, recursive = TRUE))

  outlets <- map_outlet_ids(test_data$source_outlets, test_data$reconciled)
  
  expect_equal(nrow(outlets), 15)
  
  test_data$reconciled <- test_data$reconciled[test_data$reconciled$ID %in% outlets$ID[1:10],]
  
  outlets <- map_outlet_ids(test_data$source_outlets, test_data$reconciled)
  
  expect_equal(nrow(outlets), 10)
})