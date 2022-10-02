# nolint start
library(terra)
extdata <- system.file("extdata", package = "hydrofab")

new_hope_fac <- suppressWarnings(terra::rast(file.path(extdata, "new_hope_fac.tif")))
new_hope_fdr <- suppressWarnings(terra::rast(file.path(extdata, "new_hope_fdr.tif")))

proj <- as.character(terra::crs(new_hope_fdr))

nhpgpkg <- tempfile(fileext = ".gpkg")

file.copy(file.path(extdata, "new_hope.gpkg"), nhpgpkg)

nhpgpkgref <- tempfile(fileext = ".gpkg")

file.copy(file.path(extdata, "new_hope_refactor.gpkg"), nhpgpkgref)

nhpgpkgrec <- tempfile(fileext = ".gpkg")

file.copy(file.path(extdata, "new_hope_reconcile.gpkg"), nhpgpkgrec)

nhpgpkgreccat <- tempfile(fileext = ".gpkg")

file.copy(file.path(extdata, "new_hope_cat_rec.gpkg"), nhpgpkgreccat)

nhpgpkgev <- tempfile(fileext = ".gpkg")

file.copy(file.path(extdata, "new_hope_event.gpkg"), nhpgpkgev)

nhdgpkgagg <- tempfile(fileext = ".gpkg")

file.copy(file.path(extdata, "new_hope_agg.gpkg"), nhpgpkgagg)

new_hope_catchment <- sf::read_sf(nhpgpkg, "CatchmentSP")
new_hope_catchment <- sf::st_transform(new_hope_catchment, proj)
new_hope_flowline <- sf::read_sf(nhpgpkg, "NHDFlowline_Network")
new_hope_flowline <- sf::st_transform(new_hope_flowline, proj)
new_hope_fline_ref <- sf::read_sf(nhpgpkgref)
new_hope_fline_rec <- sf::read_sf(nhpgpkgrec)
new_hope_catchment_rec <- sf::read_sf(nhpgpkgreccat)
new_hope_events <- sf::read_sf(nhpgpkgev)
new_hope_agg_flowpath <- sf::read_sf(nhdgpkgagg, "flowpath")
new_hope_agg_divides <- sf::read_sf(nhdgpkgagg, "divides")

####
# This is how the raster data was created.
# start_COMID <- 8897784
# nhdplus_path("cape_fear_nhdplus.gpkg")
# preped_nhdplus <- stage_national_data()
# flines <- readRDS(preped_nhdplus$attributes)
# UT <- get_UT(flines, start_COMID)
# new_hope_subset <- subset_nhdplus(UT, "new_hope.gpkg", overwrite = TRUE)
# flowline <- read_sf(new_hope_subset, "NHDFlowline_Network")
# 
# fac <- terra::rast("~/Documents/Projects/NWM/4_data/nhdplus_raster/fac/NHDPlusSA/NHDPlus03N/NHDPlusFdrFac03a/fac.tif")
# fdr <- terra::rast("~/Documents/Projects/NWM/4_data/nhdplus_raster/fdr/NHDPlusSA/NHDPlus03N/NHDPlusFdrFac03a/fdr.tif")
# proj <- terra::crs(fdr)
#
# catchment <- read_sf(new_hope_subset, "CatchmentSP") %>%
#   st_transform(proj)
#
# cropper <- catchment %>%
#   st_union() %>%
#   st_buffer(1000) %>%
#   as_Spatial()
#
# sub_fac <- terra::crop(fac, cropper)
# sub_fdr <- terra::crop(fdr, cropper)
# terra::writeRaster(sub_fac, "new_hope_fac.tif", overwrite = TRUE)
# terra::writeRaster(sub_fdr, "new_hope_fdr.tif", overwrite = TRUE)
# #####
#
# fac_path <- "inst/extdata/new_hope_fac.tif"
# fdr_path <- "inst/extdata/new_hope_fdr.tif"
# sub_fac <- terra::rast(fac_path)
# sub_fdr <- terra::rast(fdr_path)
# 
# proj <- terra::crs(sub_fac)
# 
# flowline <- sf::read_sf("inst/extdata/new_hope.gpkg", "NHDFlowline_Network")
# refactor_nhdplus(nhdplus_flines = flowline,
#                  split_flines_meters = 2000,
#                  collapse_flines_meters = 1000,
#                  collapse_flines_main_meters = 1000,
#                  split_flines_cores = 2,
#                  out_refactored = "inst/extdata/new_hope_refactor.gpkg",
#                  out_reconciled = "inst/extdata/new_hope_reconcile.gpkg",
#                  three_pass = TRUE,
#                  purge_non_dendritic = FALSE,
#                  warn = FALSE)
# 
# fline_ref <- sf::read_sf("inst/extdata/new_hope_refactor.gpkg") %>%
#   sf::st_transform(proj)
# fline_rec <- sf::read_sf("inst/extdata/new_hope_reconcile.gpkg") %>%
#   sf::st_transform(proj)
# 
# cat_rec <- reconcile_catchment_divides(new_hope_catchment, 
#                                        fline_ref, fline_rec,
#                                        fdr_path, fac_path)
# 
# cat_rec$area_sqkm <- as.numeric(sf::st_area(
#   sf::st_transform(cat_rec, 5070))) / (1000^2)
# 
# sf::write_sf(cat_rec, "inst/extdata/new_hope_cat_rec.gpkg")
# 
# fline_rec <- dplyr::inner_join(fline_rec,
#                                dplyr::select(sf::st_set_geometry(cat_rec, NULL),
#                                              ID, area_sqkm), by = "ID")
# fline_rec$TotDASqKM <-
#   nhdplusTools::calculate_total_drainage_area(dplyr::rename(sf::st_set_geometry(fline_rec, NULL),
#                                                             area = area_sqkm))
# 
# sf::write_sf(fline_rec, "inst/extdata/new_hope_reconcile.gpkg")
# 
# outlets <- data.frame(ID = get_id(c("8896032.1", "8896032.2", "8896032.3", "8894360,8897784")),
#                       type = c("outlet", "outlet", "outlet", "terminal"),
#                       stringsAsFactors = FALSE)
# 
# aggregated <- aggregate_to_outlets(flowpath = fline_rec,
#                                    divide = cat_rec,
#                                    outlets = outlets,
#                                    da_thresh = 2, only_larger = TRUE)
# 
# aggregated$cat_sets <- pack_set(aggregated$cat_sets)
# aggregated$fline_sets <- pack_set(aggregated$fline_sets)
# 
# sf::write_sf(aggregated$cat_sets, "inst/extdata/new_hope_agg.gpkg", "divides")
# sf::write_sf(aggregated$fline_sets, "inst/extdata/new_hope_agg.gpkg", "flowpath")
#
# nolint end
