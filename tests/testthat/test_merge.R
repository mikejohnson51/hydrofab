source(system.file("extdata/new_hope_data.R", package = "hydrofab"))

remove_gpkg_table <- function(db, table) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  
  on.exit(RSQLite::dbDisconnect(con))
  
  o <- RSQLite::dbRemoveTable(con, table)
  
  o <- RSQLite::dbSendQuery(con, sprintf("DELETE FROM gpkg_contents where table_name='%s';", table))
  
  RSQLite::dbClearResult(o)
}

refac_gpkg_1 <- tempfile(fileext = "_01.gpkg")

file.copy(new_hope_refactor, refac_gpkg_1)

test_that("generate lookup table", {
  test_gpkg <- generate_lookup_table(refactored_gpkg = refac_gpkg_1, reconciled_layer = "refactored_flowpaths")
  
  expect_equal(test_gpkg, refac_gpkg_1)
  
  expect_true("lookup_table" %in% sf::st_layers(test_gpkg)$name)
})

agg_gpkg_1 <- tempfile(fileext = "_01.gpkg")

file.copy(new_hope_agg, agg_gpkg_1)

test_that("generate lookup table agg", {
  
  remove_gpkg_table(agg_gpkg_1, "lookup_table")
  
  expect_false("lookup_table" %in% sf::st_layers(agg_gpkg_1)$name)
  
  test_gpkg <- generate_lookup_table(gpkg = agg_gpkg_1, refactored_gpkg = refac_gpkg_1)
  
  expect_equal(test_gpkg, agg_gpkg_1)
  
  expect_true("lookup_table" %in% sf::st_layers(test_gpkg)$name)
})

refac_gpkg_2 <- tempfile(fileext = "_02.gpkg")

file.copy(refac_gpkg_1, refac_gpkg_2)

agg_gpkg_2 <- tempfile(fileext = "_02.gpkg")

file.copy(agg_gpkg_1, agg_gpkg_2)

test_that("assign global identifiers", {  
  
  in1 <- hydrofab::read_hydrofabric(refac_gpkg_1, verbose = FALSE)
  in2 <- hydrofab::read_hydrofabric(refac_gpkg_2, verbose = FALSE)
  
  out <- assign_global_identifiers(c(refac_gpkg_1, refac_gpkg_2), 
                            flowpath_layer = "refactored_flowpaths", 
                            divide_layer = "refactored_divides", 
                            mapped_POI_layer = "mapped_POIs", 
                            lookup_table_layer = "lookup_table", 
                            catchment_network_layer = "catchment_network", 
                            overwrite = TRUE)
  
  expect_equal(names(out), c("meta", "lookup"))

  expect_true(!all(duplicated(out$lookup$newID)))  
  
  expect_equal(seq((nrow(in1$flowpaths) + nrow(in2$flowpaths))), out$lookup$newID)
  
  expect_equal(unique(out$lookup$VPU), c("01", "02"))
  
  in1 <- hydrofab::read_hydrofabric(agg_gpkg_1, verbose = FALSE)
  in2 <- hydrofab::read_hydrofabric(agg_gpkg_2, verbose = FALSE)
  
  out <- assign_global_identifiers(c(agg_gpkg_1, agg_gpkg_2), 
                                   flowpath_layer = "flowpath", 
                                   divide_layer = "divides", 
                                   mapped_POI_layer = "mapped_POIs", 
                                   lookup_table_layer = "lookup_table", 
                                   catchment_network_layer = "catchment_network", 
                                   overwrite = TRUE, verbose = FALSE)
  
  expect_equal(seq((nrow(in1$flowpaths) + nrow(in2$flowpaths))), out$lookup$newID)
  
})
