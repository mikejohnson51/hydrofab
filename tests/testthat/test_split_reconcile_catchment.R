context("split_catchment_divide")

Sys.setenv(TURN_OFF_SYS_MAPSHAPER = "YUP")

test_that("split_catchment_divide works", {
  tf <- file.path(tempfile(fileext = ".gpkg"))
  tr <- file.path(tempfile(fileext = ".gpkg"))

  source(system.file("extdata", "walker_data.R", package = "hydrofab"))

  refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
                               split_flines_meters = 2000,
                               collapse_flines_meters = 1,
                               collapse_flines_main_meters = 1,
                               split_flines_cores = 1,
                               out_refactored = tf,
                               out_reconciled = tr,
                               three_pass = TRUE,
                               purge_non_dendritic = FALSE,
                               warn = FALSE)

  fline_ref <- sf::read_sf(tf) %>%
    dplyr::arrange(COMID)
  
  fline_rec <- sf::read_sf(tr)

  test_flines <- dplyr::filter(fline_ref, as.integer(COMID) == 5329435)

  test_cat <- dplyr::filter(walker_catchment, FEATUREID == 5329435)

  expect_true(nrow(test_flines) == 5, "got wrong number of test_flines")
  
  test_cat = st_transform(test_cat, st_crs(walker_fdr))
  test_flines = st_transform(test_flines, st_crs(walker_fdr))

  suppressWarnings(split_cat <- split_catchment_divide(test_cat, 
                                                       test_flines, 
                                                       walker_fdr, 
                                                       walker_fac))
  
  expect_true(length(split_cat) == 5, "Got the wrong number of cathment split polygons")
  expect_true(all(c("XY", "MULTIPOLYGON", "sfg") %in% class(split_cat[[5]])),
         "Got wrong class for polygon with pixel dribble out the bottom")
  expect_true(all(c("XY", "POLYGON", "sfg") %in% class(split_cat[[2]])),
         "Got wrong class for polygon with one ring")

  split_cat <- split_catchment_divide(test_cat, test_flines, 
                                      walker_fdr, walker_fac, lr = TRUE)
  
  expect_true(length(split_cat) == 5, "Got the wrong number of cathment split polygons")
  
  expect_true(all(lengths(st_geometry(split_cat)) == 2))
  
  test_fline_ref <- fline_ref[1:9, ] # this sucks, but works.
  test_fline_rec <- dplyr::filter(fline_rec, member_COMID %in% as.character(test_fline_ref$COMID))

  test_cat <- dplyr::filter(walker_catchment,
                  FEATUREID %in% unique(as.integer(test_fline_ref$COMID)))

  test_cat = st_transform(test_cat, st_crs(walker_fdr))
  test_fline_ref = st_transform(test_fline_ref, st_crs(walker_fdr))
  test_fline_rec = st_transform(test_fline_rec, st_crs(walker_fdr))
  
  suppressWarnings(reconciled_cats <- reconcile_catchment_divides(catchment = test_cat, 
                                                                  fline_ref = test_fline_ref, 
                                                                  fline_rec = test_fline_rec,
                                                                  fdr = walker_fdr, 
                                                                  fac = walker_fac, 
                                                                  para = 1))

  expect_true(nrow(reconciled_cats) == nrow(test_fline_ref), "got the wrong number of split catchments")
  expect_true(all(reconciled_cats$member_COMID %in% test_fline_ref$COMID))

})

test_that("split catchment divide left right", {
  source(system.file("extdata", "walker_data.R", package = "hydrofab"))
  
  cat   <- dplyr::filter(walker_catchment, FEATUREID == 5329357)
  fline <- dplyr::filter(walker_flowline, COMID == 5329357)
  
  cat = st_transform(cat, st_crs(walker_fdr))
  fline = st_transform(fline, st_crs(walker_fdr))
  
  sc <- split_catchment_divide(cat, fline, walker_fdr, walker_fac, lr = TRUE)
  
  expect_is(sc, "sfc_MULTIPOLYGON")
  
  expect_equal(length(sc[[1]]), 2)
  
  cat <- dplyr::filter(walker_catchment, FEATUREID == 5329435)
  fline <- dplyr::filter(walker_flowline, COMID == 5329435)
  
  cat = st_transform(cat, st_crs(walker_fdr))
  fline = st_transform(fline, st_crs(walker_fdr))
  
  sc <- split_catchment_divide(cat, fline, walker_fdr, walker_fac, lr = TRUE)
  
  expect_is(sc, "sfc_MULTIPOLYGON")
  
  expect_equal(length(sc[[1]]), 2)
})

test_that("split and reconcile works", {

  source(system.file("extdata", "walker_data.R", package = "hydrofab"))

  out_col <- tempfile(fileext = ".gpkg")
  out_rec <- tempfile(fileext = ".gpkg")
  
  refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
                               split_flines_meters = 2000,
                               collapse_flines_meters = 1000,
                               collapse_flines_main_meters = 1000,
                               split_flines_cores = 2,
                               out_refactored = out_col,
                               out_reconciled = out_rec,
                               three_pass = TRUE,
                               purge_non_dendritic = FALSE,
                               warn = FALSE, exclude_cats = 5329305)

  fline_ref <- sf::read_sf(out_col) %>%
    dplyr::arrange(COMID)
  fline_rec <- sf::read_sf(out_rec)
  
  # 5329305 would get split if not included in exclude_cats
  expect_true("5329293,5329305" %in% fline_rec$member_COMID)
  
  unlink(out_col)
  unlink(out_rec)
  
  test_cat_1 <- fline_rec$member_COMID[which(nchar(fline_rec$member_COMID) ==
                                              max(nchar(fline_rec$member_COMID)))]
  test_cat_2 <- fline_rec$member_COMID[which(fline_rec$ID == 2)]

  test_cat_list <- c(unlist(strsplit(test_cat_1, ",")), unlist(strsplit(test_cat_2, ",")))

  test_cat <- dplyr::filter(walker_catchment, FEATUREID %in% as.integer(test_cat_list))

  test_fline_ref <- dplyr::filter(fline_ref, as.integer(COMID) %in%
                                    as.integer(test_cat_list))

  test_fline_rec <- dplyr::filter(fline_rec, member_COMID %in%
                                     c(test_cat_1, test_cat_2))

  test_cat = st_transform(test_cat, st_crs(walker_fdr))
  test_fline_ref = st_transform(test_fline_ref, st_crs(walker_fdr))
  
  reconciled_cats <- reconcile_catchment_divides(test_cat, test_fline_ref,
                                          test_fline_rec, walker_fdr, walker_fac, 
                                          para = 1)

  expect_true(nrow(reconciled_cats) == nrow(test_fline_rec))
  expect_true(all(reconciled_cats$member_COMID %in% test_fline_rec$member_COMID))
})


test_that("reconcile catchments works with reconciled flowline from split", {

  # "166755072,8866562.2"
  # "8833300.1", "8833300.2"

  fdr <- suppressWarnings(terra::rast(
    list.files(pattern = "reconcile_test_fdr.tif$", 
               recursive = TRUE, full.names = TRUE)))
  
  fac <- suppressWarnings(terra::rast(
    list.files(pattern = "reconcile_test_fac.tif$",
               recursive = TRUE, full.names = TRUE)))
  
  raster_proj <- sf::st_crs(fdr)
  
  rec_gpkg <- list.files(pattern = "reconcile_test.gpkg",
             recursive = TRUE, full.names = TRUE)
  
  test_fline_ref <- st_transform(
    sf::read_sf(rec_gpkg, "fline_ref"), raster_proj) %>%
    dplyr::filter(!grepl("8833300", COMID))
  
  test_fline_rec <- st_transform(
    sf::read_sf(rec_gpkg, "fline_rec"), raster_proj) %>%
    dplyr::filter(!ID %in% c(7912, 7913))
  
  test_cat <- st_transform(
    sf::read_sf(rec_gpkg, "catchment"), raster_proj) %>%
    dplyr::filter(FEATUREID != 8833300)

  suppressWarnings(reconciled_cats <- reconcile_catchment_divides(catchment = test_cat, 
                                                                  fline_ref = test_fline_ref,
                                                                  fline_rec = test_fline_rec, 
                                                                  fdr, 
                                                                  fac, 
                                                                  para = 1))

  expect_true(nrow(reconciled_cats) == nrow(test_fline_rec),
         "Got the wrong number of reconciled catchments")

  expect_true(all(reconciled_cats$member_COMID %in% test_fline_rec$member_COMID))

  expect_false("8833300.1" %in% reconciled_cats$member_COMID)
  expect_true("166755072,8866562.2" %in% reconciled_cats$member_COMID)
})


test_that("doing nothing does nothing", {
  unlink("data/temp/*")
  dir.create("data/temp", showWarnings = FALSE, recursive = TRUE)

  source(system.file("extdata", "walker_data.R", package = "hydrofab"))

  refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
                               split_flines_meters = 200000,
                               collapse_flines_meters = 1,
                               collapse_flines_main_meters = 1,
                               split_flines_cores = 2,
                               out_refactored = "data/temp/subset_refactor.gpkg",
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

test_that("too small split", {

  # Tests the snap_distance_m parameter.
  catchment <- read_sf(list.files(pattern = "split_bug.gpkg$", full.names = TRUE, recursive = TRUE), "catchment")
  fline <- read_sf(list.files(pattern = "split_bug.gpkg$", full.names = TRUE, recursive = TRUE), "fline")

  fdr <- rast( list.files(pattern = "split_bug_fdr.tif$", full.names = TRUE, recursive = TRUE))
  fac <- rast( list.files(pattern = "split_bug_fac.tif$", full.names = TRUE, recursive = TRUE))

  expect_error(split_catchment_divide(catchment, fline, fdr, fac,
                                      min_area_m = 800, snap_distance_m = 100),
               "Nothing left over. Split too small?")

  out <- split_catchment_divide(catchment, fline, fdr, fac,
                                min_area_m = 800, snap_distance_m = 40)

  expect_equal(length(out), 2)
})

test_that("no fdr", {
  
  tf <- file.path(tempfile(fileext = ".gpkg"))
  tr <- file.path(tempfile(fileext = ".gpkg"))
  
  unlink(tf)
  unlink(tr)
  
  source(system.file("extdata", "walker_data.R", package = "hydrofab"))
  
  refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
                               split_flines_meters = 2000000,
                               collapse_flines_meters = 2000,
                               collapse_flines_main_meters = 1000,
                               split_flines_cores = 2,
                               out_refactored = tf,
                               out_reconciled = tr,
                               three_pass = TRUE,
                               purge_non_dendritic = FALSE,
                               warn = FALSE)
  
  fline_ref <- sf::read_sf(tf) %>%
    dplyr::arrange(COMID)
  
  fline_rec <- sf::read_sf(tr)
  
  cat <- sf::st_transform(walker_catchment, sf::st_crs(fline_rec))
  
  reconcile <- reconcile_catchment_divides(catchment = walker_catchment, 
                                           fline_ref, 
                                           fline_rec)
  
  expect_equal(nrow(reconcile), 42)
  
})

test_that("merrit dem", {
  
  fdr <- terra::rast(list.files(pattern = "ak_fdr.tif$", full.names = TRUE, recursive = TRUE))
  fac <- terra::rast(list.files(pattern = "ak_fac.tif$", full.names = TRUE, recursive = TRUE))
  cat <- sf::read_sf(list.files(pattern = "ak_vector.gpkg$", full.names = TRUE, recursive = TRUE), "catchment")
  ref <- sf::read_sf(list.files(pattern = "ak_vector.gpkg$", full.names = TRUE, recursive = TRUE), "fline_ref")
  rec <- sf::read_sf(list.files(pattern = "ak_vector.gpkg$", full.names = TRUE, recursive = TRUE), "fline_rec")

  rec_cat <- suppressWarnings(reconcile_catchment_divides(
    catchment = cat,
    fline_ref =     ref,
    fline_rec =     rec,
    fdr,
    fac,
    para = 1, 
    min_area_m = 10000,
    snap_distance_m = 5,
    simplify_tolerance_m = 5,
    vector_crs = 3338))

  expect_equal(rec_cat$member_COMID, c("81000012.1", "81000012.2"))

  # plot(rec_cat$geom[2])
  # plot(rec_cat$geom[1], add = TRUE, col = "red")

})

Sys.unsetenv("TURN_OFF_SYS_MAPSHAPER")
