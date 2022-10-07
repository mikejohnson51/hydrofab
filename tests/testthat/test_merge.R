source(system.file("extdata/new_hope_data.R", package = "hydrofab"))

Sys.setenv("hydrofab_verbose" = "false")

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
  
  sf::st_delete(agg_gpkg_1, "lookup_table", quiet = TRUE)
  
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
                            overwrite = TRUE, 
                            verbose = FALSE)
  
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
  
  agg_gpkg_3 <- tempfile(fileext = "_14.gpkg")
  agg_gpkg_4 <- tempfile(fileext = "_15.gpkg")
  
  file.copy(agg_gpkg_1, agg_gpkg_3)
  file.copy(agg_gpkg_1, agg_gpkg_4)
  
  # VPUID toVPUID    COMID  toCOMID
  # 1    14      15 20734037 20734041
  # 2    14      15 18267749 20734041
  
  lu <- sf::read_sf(agg_gpkg_3, "lookup_table")
  
  lu$NHDPlusV2_COMID[1] <- 20734037
  lu$NHDPlusV2_COMID[500] <- 18267749
  
  sf::write_sf(lu, agg_gpkg_3, "lookup_table")
  
  lu <- sf::read_sf(agg_gpkg_4, "lookup_table")
  
  lu$NHDPlusV2_COMID[1] <- 20734041
  
  sf::write_sf(lu, agg_gpkg_4, "lookup_table")
  
  out <- assign_global_identifiers(c(agg_gpkg_3, agg_gpkg_4), 
                                   flowpath_layer = "flowpath", 
                                   divide_layer = "divides", 
                                   mapped_POI_layer = "mapped_POIs", 
                                   lookup_table_layer = "lookup_table", 
                                   catchment_network_layer = "catchment_network", 
                                   overwrite = TRUE, verbose = FALSE)
  
  
})


