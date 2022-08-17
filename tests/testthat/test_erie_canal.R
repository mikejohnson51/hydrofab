
# nhdplus_path("~/Documents/Data/nhdp/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb/")
# nhd <- stage_national_data()
# atts <- readRDS(nhd$attributes)
# UT <- c(get_UT(atts, 21976351), get_UT(atts, 21972746))
#
#
# nhd <- nhdplusTools::subset_nhdplus(UT,
#                                     output_file = "data/erie.gpkg",
#                                     return_data = TRUE)
#
# download_fdr_fac("data", regions = "04a")
#
# fac <- terra::rast("~/Documents/active_code/gfv2/workspace/data/fdrfac/NHDPlusGL/NHDPlus04/NHDPlusFdrFac04a/fac/")
# fdr <- terra::rast("~/Documents/active_code/gfv2/workspace/data/fdrfac/NHDPlusGL/NHDPlus04/NHDPlusFdrFac04a/fdr/")
# proj <- as.character(terra::crs(fdr))
#
# catchment <- nhd$CatchmentSP %>%
#   st_transform(proj)
#
# cropper <- catchment %>%
#   st_union() %>%
#   st_buffer(1000) %>%
#   as_Spatial()
#
# sub_fac <- terra::crop(fac, cropper)
# sub_fdr <- terra::crop(fdr, cropper)
# terra::writeRaster(sub_fac, "data/erie_fac.tif", overwrite = TRUE)
# terra::writeRaster(sub_fdr, "data/erie_fdr.tif", overwrite = TRUE)
#####
context("erie")
test_that("Erie Canal area works", {
fac <- terra::rast(list.files(pattern = "erie_fac.tif$", full.names = TRUE, recursive = TRUE))
fdr <- terra::rast(list.files(pattern = "erie_fdr.tif$", full.names = TRUE, recursive = TRUE))

gpkg <- list.files(pattern = "erie.gpkg$", full.names = TRUE, recursive = TRUE)

flowline <- sf::read_sf(gpkg, "NHDFlowline_Network") %>%
  sf::st_transform(terra::crs(fac))
catchment <- sf::read_sf(gpkg, "CatchmentSP") %>%
  sf::st_transform(terra::crs(fac))

catchment <- sf::st_make_valid(catchment)

out_refactor <- tempfile(fileext = ".gpkg")
out_rec <- tempfile(fileext = ".gpkg")

flowline <- dplyr::right_join(dplyr::select(flowline, COMID),
                       nhdplusTools::prepare_nhdplus(flowline, 0, 0, 0, FALSE, warn = FALSE),
                       by = "COMID")

# library(RSQLite)
# con <- dbConnect(SQLite(), dbname = "../../../gfv2/workspace/cache/04a.gpkg")
# outlets <- RSQLite::dbReadTable(con, "outlets")
# dbDisconnect(con)
#
# outlets <- filter(outlets, ID %in% flowline$COMID)
#
# saveRDS(outlets, "data/erie_outlets.rds")

outlets <- readRDS(list.files(pattern = "erie_outlets.rds$", full.names = TRUE, recursive = TRUE))

refactor_nhdplus(nhdplus_flines = flowline,
                 split_flines_meters = 10000,
                 collapse_flines_meters = 1000,
                 collapse_flines_main_meters = 1000,
                 split_flines_cores = 1,
                 out_refactored = out_refactor,
                 out_reconciled = out_rec,
                 three_pass = TRUE,
                 purge_non_dendritic = FALSE,
                 warn = FALSE, exclude_cats = outlets$ID)


fline_ref <- sf::read_sf(out_refactor) %>%
  sf::st_transform(terra::crs(fac))

fline_rec <- sf::read_sf(out_rec) %>%
  sf::st_transform(terra::crs(fac))

suppressWarnings(cat_rec <- reconcile_catchment_divides(catchment,
                                                        fline_ref,
                                                        fline_rec,
                                                        fdr,
                                                        fac,
                                                        para = 1))


fline_rec$member_COMID <- strsplit(fline_rec$member_COMID, ",")
fline_rec$member_COMID <- lapply(fline_rec$member_COMID, as.integer)

ids <- sapply(outlets$ID,
              function(x, fline) {
                fline$ID[sapply(fline$member_COMID,
                                function(l, y) y %in% l, y = x)]
              }, fline = fline_rec)

outlets <- outlets[lengths(ids) > 0, ]

ids <- ids[lengths(ids) > 0]

ids <- lapply(ids, function(x) x[[1]])

outlets$ID <- as.integer(ids)

outlets_sub <- dplyr::filter(outlets, !is.na(ID) & ID %in% cat_rec$ID)

cat_agg <- aggregate_to_outlets(flowpath =  fline_rec,
                                divide = cat_rec,
                                outlets = outlets_sub,
                                da_thresh = 1,
                                only_larger = TRUE)

expect_equal(nrow(cat_agg$cat_sets), 517)

expect_equal(nrow(cat_agg$fline_sets), 517)
})
