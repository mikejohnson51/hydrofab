context("split_catchment_divides")

test_that("split_catchment_divides works", {
  unlink("data/temp/*")
  dir.create("data/temp", showWarnings = FALSE, recursive = TRUE)

  source(system.file("extdata", "walker_data.R", package = "hyRefactor"))

  refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
                               split_flines_meters = 2000,
                               collapse_flines_meters = 1,
                               collapse_flines_main_meters = 1,
                               split_flines_cores = 2,
                               out_collapsed = "data/temp/subset_refactor.gpkg",
                               out_reconciled = "data/temp/subset_reconcile.gpkg",
                               three_pass = TRUE,
                               purge_non_dendritic = FALSE,
                               warn = FALSE)

  fline_ref <- sf::read_sf("data/temp/subset_refactor.gpkg") %>%
    dplyr::arrange(COMID)
  fline_rec <- sf::read_sf("data/temp/subset_reconcile.gpkg")

  test_flines <- dplyr::filter(fline_ref, as.integer(COMID) == 5329435)

  test_cat <- dplyr::filter(walker_catchment, FEATUREID == 5329435)

  expect_true(nrow(test_flines) == 5, "got wrong number of test_flines")

  split_cat <- split_catchment_divide(test_cat, test_flines, 
                                      walker_fdr, walker_fac)

  expect_true(length(split_cat) == 5, "Got the wrong number of cathment split polygons")
  expect_true(all(c("XY", "MULTIPOLYGON", "sfg") %in% class(split_cat[[5]])),
         "Got wrong class for polygon with pixel dribble out the bottom")
  expect_true(all(c("XY", "POLYGON", "sfg") %in% class(split_cat[[2]])),
         "Got wrong class for polygon with one ring")

  test_fline_ref <- fline_ref[1:9, ] # this sucks, but works.
  test_fline_rec <- dplyr::filter(fline_rec,
                              member_COMID %in% as.character(test_fline_ref$COMID))

  test_cat <- dplyr::filter(walker_catchment,
                  FEATUREID %in% unique(as.integer(test_fline_ref$COMID)))

  reconciled_cats <- reconcile_catchment_divides(test_cat, test_fline_ref, test_fline_rec,
                                          walker_fdr, walker_fac, para = 2)

  expect_true(nrow(reconciled_cats) == nrow(test_fline_ref), "got the wrong number of split catchments")
  expect_true(all(reconciled_cats$member_COMID %in% test_fline_ref$COMID))

  unlink("data/temp/*")
})

test_that("split and reconcile works", {
  unlink("data/temp/*")
  dir.create("data/temp", showWarnings = FALSE, recursive = TRUE)

  source(system.file("extdata", "walker_data.R", package = "hyRefactor"))

  refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
                               split_flines_meters = 2000,
                               collapse_flines_meters = 1000,
                               collapse_flines_main_meters = 1000,
                               split_flines_cores = 2,
                               out_collapsed = "data/temp/subset_refactor.gpkg",
                               out_reconciled = "data/temp/subset_reconcile.gpkg",
                               three_pass = TRUE,
                               purge_non_dendritic = FALSE,
                               warn = FALSE)

  fline_ref <- sf::read_sf("data/temp/subset_refactor.gpkg") %>%
    dplyr::arrange(COMID)
  fline_rec <- sf::read_sf("data/temp/subset_reconcile.gpkg")

  test_cat_1 <- fline_rec$member_COMID[which(nchar(fline_rec$member_COMID) ==
                                              max(nchar(fline_rec$member_COMID)))]
  test_cat_2 <- fline_rec$member_COMID[which(fline_rec$ID == 2)]

  test_cat_list <- c(unlist(strsplit(test_cat_1, ",")), unlist(strsplit(test_cat_2, ",")))

  test_cat <- dplyr::filter(walker_catchment, FEATUREID %in% as.integer(test_cat_list))

  test_fline_ref <- dplyr::filter(fline_ref, as.integer(COMID) %in%
                                    as.integer(test_cat_list))

  test_fline_rec <- dplyr::filter(fline_rec, member_COMID %in%
                                     c(test_cat_1, test_cat_2))

  reconciled_cats <- reconcile_catchment_divides(test_cat, test_fline_ref,
                                          test_fline_rec, walker_fdr, walker_fac, 
                                          para = 2)

  expect_true(nrow(reconciled_cats) == nrow(test_fline_rec))
  expect_true(all(reconciled_cats$member_COMID %in% test_fline_rec$member_COMID))

  unlink("data/temp/*")
})


test_that("reconcile catchments works with reconciled flowline from split", {

  # "166755072,8866562.2"
  # "8833300.1", "8833300.2"

  fdr <- raster::raster("data/reconcile_test_fdr.tif")
  fac <- raster::raster("data/reconcile_test_fac.tif")
  test_fline_ref <- sf::read_sf("data/reconcile_test.gpkg", "fline_ref")
  test_fline_rec <- sf::read_sf("data/reconcile_test.gpkg", "fline_rec")
  test_cat <- sf::read_sf("data/reconcile_test.gpkg", "catchment")

  reconciled_cats <- reconcile_catchment_divides(test_cat, test_fline_ref,
                                          test_fline_rec, fdr, fac, para = 2)

  expect_true(nrow(reconciled_cats) == nrow(test_fline_rec) - 1,
         "Got the wrong number of reconciled catchments")

  expect_true(all(reconciled_cats$member_COMID %in% test_fline_rec$member_COMID))

  expect_true("8833300.1" %in% reconciled_cats$member_COMID)
  expect_true("166755072,8866562.2" %in% reconciled_cats$member_COMID)
})


test_that("doing nothing does nothing", {
  unlink("data/temp/*")
  dir.create("data/temp", showWarnings = FALSE, recursive = TRUE)

  source(system.file("extdata", "walker_data.R", package = "hyRefactor"))

  refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
                               split_flines_meters = 200000,
                               collapse_flines_meters = 1,
                               collapse_flines_main_meters = 1,
                               split_flines_cores = 2,
                               out_collapsed = "data/temp/subset_refactor.gpkg",
                               out_reconciled = "data/temp/subset_reconcile.gpkg",
                               three_pass = TRUE,
                               purge_non_dendritic = FALSE,
                               warn = FALSE)

  fline_ref <- sf::read_sf("data/temp/subset_refactor.gpkg") %>%
    dplyr::arrange(COMID)
  fline_rec <- sf::read_sf("data/temp/subset_reconcile.gpkg")

  expect_true(nrow(fline_ref) == nrow(fline_rec))
  expect_true(nrow(fline_ref) == nrow(walker_catchment))

  unlink("data/temp", recursive = TRUE)
})
