source(system.file("extdata/new_hope_data.R", package = "hydrofab"))

Sys.setenv("hydrofab_verbose" = "false")

test_that("generate lookup table", {
  refac_gpkg_1 <- tempfile(fileext = "_01.gpkg")
  
  file.copy(new_hope_refactor, refac_gpkg_1)
  
  test_gpkg <- add_lookup_table(refactored_gpkg = refac_gpkg_1, 
                                reconciled_layer = "refactored_flowpaths")
  
  expect_equal(test_gpkg, refac_gpkg_1)
  
  expect_true("lookup_table" %in% sf::st_layers(test_gpkg)$name)
  
  lookup_table <- sf::read_sf(refac_gpkg_1, "lookup_table")
  
  expect_equal(names(lookup_table), c("NHDPlusV2_COMID",
                                      "reconciled_ID",
                                      "member_COMID"))
  
  unlink(refac_gpkg_1)
})

test_that("generate lookup table agg", {
  
  refac_gpkg_1 <- tempfile(fileext = "_01.gpkg")
  
  file.copy(new_hope_refactor, refac_gpkg_1)

  agg_gpkg_1 <- tempfile(fileext = "_01.gpkg")
  
  file.copy(new_hope_agg, agg_gpkg_1)
    
  sf::st_delete(agg_gpkg_1, "lookup_table", quiet = TRUE)
  
  expect_false("lookup_table" %in% sf::st_layers(agg_gpkg_1)$name)
  
  test_gpkg <- add_lookup_table(gpkg = agg_gpkg_1, refactored_gpkg = refac_gpkg_1)
  
  expect_equal(test_gpkg, agg_gpkg_1)
  
  expect_true("lookup_table" %in% sf::st_layers(test_gpkg)$name)
  
  lookup_table <- sf::read_sf(test_gpkg, "lookup_table")
  
  expect_equal(names(lookup_table), c("NHDPlusV2_COMID", "NHDPlusV2_COMID_part", 
                                      "reconciled_ID", "aggregated_ID", "toID", 
                                      "mainstem", 
                                      "POI_ID", "POI_TYPE", "POI_VALUE"))
  
  unlink(agg_gpkg_1)
  unlink(refac_gpkg_1)
})

test_that("assign global identifiers refactor", {  
  
  refac_gpkg_1 <- tempfile(fileext = "_01.gpkg")
  
  file.copy(new_hope_refactor, refac_gpkg_1)
  
  refac_gpkg_2 <- tempfile(fileext = "_02.gpkg")
  
  file.copy(refac_gpkg_1, refac_gpkg_2)
  
  in1 <- read_hydrofabric(refac_gpkg_1, verbose = FALSE)
  in2 <- read_hydrofabric(refac_gpkg_2, verbose = FALSE)
  
  out <- assign_global_identifiers(c(refac_gpkg_1, refac_gpkg_2), 
                            flowpath_layer = "refactored_flowpaths", 
                            divide_layer = "refactored_divides", 
                            mapped_POI_layer = "mapped_POIs", 
                            lookup_table_layer = "lookup_table", 
                            flowpath_edge_list = "catchment_network", 
                            overwrite = TRUE, 
                            verbose = FALSE)
  
  expect_equal(names(out), c("meta", "lookup"))

  expect_true(!all(duplicated(out$lookup$newID)))  
  
  expect_equal(seq((nrow(in1$flowpaths) + nrow(in2$flowpaths))), out$lookup$newID)
  
  expect_equal(unique(out$lookup$VPU), c("01", "02"))
  
  unlink(refac_gpkg_1)
  unlink(refac_gpkg_2)
})

test_that("assign global identifiers aggregated", {
  
  agg_gpkg_1 <- tempfile(fileext = "_01.gpkg")
  file.copy(new_hope_agg, agg_gpkg_1)
  
  lu <- sf::read_sf(agg_gpkg_1, "lookup_table")
  
  lu$member_comid <- paste0(lu$NHDPlusV2_COMID, lu$NHDPlusV2_COMID_part)
  
  sf::write_sf(lu, agg_gpkg_1, "lookup_table")
  
  agg_gpkg_2 <- tempfile(fileext = "_02.gpkg")
  file.copy(new_hope_agg, agg_gpkg_2)
  
  in1 <- read_hydrofabric(agg_gpkg_1, verbose = FALSE)
  in2 <- read_hydrofabric(agg_gpkg_2, verbose = FALSE)
  
  out <- assign_global_identifiers(c(agg_gpkg_1, agg_gpkg_2), 
                                   flowpath_layer = "flowpath", 
                                   divide_layer = "divides", 
                                   mapped_POI_layer = "mapped_POIs", 
                                   lookup_table_layer = "lookup_table", 
                                   flowpath_edge_list = "catchment_network", 
                                   overwrite = TRUE, verbose = FALSE)
  
  expect_equal(seq((nrow(in1$flowpaths) + nrow(in2$flowpaths))), out$lookup$newID)
  
  agg_gpkg_1 <- tempfile(fileext = "_14.gpkg")
  agg_gpkg_2 <- tempfile(fileext = "_15.gpkg")
  
  file.copy(new_hope_agg, agg_gpkg_1)
  file.copy(new_hope_agg, agg_gpkg_2)
  
  lu <- sf::read_sf(agg_gpkg_1, "lookup_table")
  
  # These were chosen by hand 
  # numbers assigned are based on this table
  # VPUID toVPUID    COMID  toCOMID
  # 1    14      15 20734037 20734041
  # 2    14      15 18267749 20734041
  lu$NHDPlusV2_COMID[lu$NHDPlusV2_COMID == 8894342] <- 20734037
  lu$NHDPlusV2_COMID[lu$NHDPlusV2_COMID == 8896272] <- 18267749
  
  sf::write_sf(lu, agg_gpkg_1, "lookup_table")
  
  # NOTE: aggregated_ID  and some others below attribute here may change
  orig_id_1 <- lu$aggregated_ID[lu$NHDPlusV2_COMID == 20734037]
  orig_id_2 <- lu$aggregated_ID[lu$NHDPlusV2_COMID == 18267749]
  
  # check the network topology as is -- in a real case, these would be toid = 0 
  # but we are just mocking this and making sure the code changes as expected.
  network <- sf::read_sf(agg_gpkg_1, "flowpath")
   
  orig_toid_1 <- network$toID[network$ID == orig_id_1]
  orig_toid_2 <- network$toID[network$ID == orig_id_2]
  
  # now modify the downstream one that it should adjust to make things point to
  lu <- sf::read_sf(agg_gpkg_2, "lookup_table")
  
  # this comid is the toCOMID from the table above.
  lu$NHDPlusV2_COMID[lu$NHDPlusV2_COMID == 8897784] <- 20734041
  
  sf::write_sf(lu, agg_gpkg_2, "lookup_table")
  
  out <- assign_global_identifiers(c(agg_gpkg_1, agg_gpkg_2), 
                                   flowpath_layer = "flowpath", 
                                   divide_layer = "divides", 
                                   mapped_POI_layer = "mapped_POIs", 
                                   lookup_table_layer = "lookup_table", 
                                   flowpath_edge_list = "catchment_network", 
                                   overwrite = TRUE, verbose = FALSE)
  
  check_1 <- sf::read_sf(agg_gpkg_1, "flowpath")
  check_1_lu <- sf::read_sf(agg_gpkg_1, "lookup_table")
  
  new_id_1 <- check_1_lu$aggregated_flowpath_ID[check_1_lu$NHDPlusV2_COMID == 20734037]
  new_id_2 <- check_1_lu$aggregated_flowpath_ID[check_1_lu$NHDPlusV2_COMID == 18267749]
  
  check_2_lu <- sf::read_sf(agg_gpkg_2, "lookup_table")
  
  # now make sure that things got repointed correctly.
  expect_equal(check_1$toid[check_1$id == new_id_1], 
               check_2_lu$aggregated_flowpath_ID[check_2_lu$NHDPlusV2_COMID == 20734041])
  expect_equal(check_1$toid[check_1$id == new_id_2], 
               check_2_lu$aggregated_flowpath_ID[check_2_lu$NHDPlusV2_COMID == 20734041])
  
  unlink(agg_gpkg_1)
  unlink(agg_gpkg_2)
  
  agg_gpkg_1 <- tempfile(fileext = "_10U.gpkg")
  agg_gpkg_2 <- tempfile(fileext = "_10L.gpkg")
  
  file.copy(new_hope_agg, agg_gpkg_1)
  file.copy(new_hope_agg, agg_gpkg_2)
  
  expect_error(
  out <- assign_global_identifiers(c(agg_gpkg_1, agg_gpkg_2), 
                                   flowpath_layer = "flowpath", 
                                   divide_layer = "divides", 
                                   mapped_POI_layer = "mapped_POIs", 
                                   lookup_table_layer = "lookup_table", 
                                   flowpath_edge_list = "catchment_network", 
                                   overwrite = TRUE, verbose = FALSE))

  unlink(agg_gpkg_1)
  unlink(agg_gpkg_2)
})


