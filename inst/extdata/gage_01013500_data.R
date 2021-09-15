# # nolint start
# library(raster)
# library(dplyr)
# library(sf)
# library(nhdplusTools)
# 
# extdata   <- system.file("extdata", package = "hyRefactor")
# 
# # gage_01013500.gpkg turned into pre-processed sample data.
# base      = "/Volumes/Transcend/ngen/refactor-tests/base-data/"
# # RDS NHD files (dont keep local for space)
# cats      = readRDS(file.path(base, "catchments_all.rds"))
# fps       = readRDS(file.path(base, "nhdplus_flowline_update_sb.rds"))
# 
# UT_COMIDs = get_UT(st_drop_geometry(fps), 724696)
# flowpaths = make_standalone(filter(fps, COMID %in% UT_COMIDs))
# catchments = filter(cats, FEATUREID %in% UT_COMIDs)
# 
# fac <- raster::raster(file.path(base, "fdrfac/01a_fac.tif")) 
# 
# fac = crop(fac, st_transform(catchments, crs(fac)), snap = "out")
# 
# fdr = raster(file.path(base, "fdrfac/01a_fdr.tif")) %>%
#   crop(st_transform(catchments, crs(fac)), snap = "out")
# 
# 
# refactor_nhdplus(nhdplus_flines = flowpaths,
#                              split_flines_meters         = 10000,
#                              collapse_flines_meters      = 1000,
#                              collapse_flines_main_meters = 1000,
#                              split_flines_cores          = 2,
#                              out_refactored              = "refactor.gpkg",
#                              out_reconciled              = "reconcile.gpkg",
#                              three_pass                  = TRUE,
#                              purge_non_dendritic         = FALSE,
#                              warn                        = FALSE)
# 
# fline_ref <-  st_transform(read_sf("refactor.gpkg"), st_crs(fac))
# fline_rec <-  st_transform(read_sf("reconcile.gpkg"), st_crs(fac))
# 
# catchments = st_transform(catchments, st_crs(fac))
# st_precision(catchments) <- raster::res(fdr)[1]
# 
# divides_old   <- reconcile_catchment_divides(catchment = catchments,
#                                               fline_ref = fline_ref,
#                                               fline_rec = fline_rec,
#                                               fdr       = fdr,
#                                               fac       = fac,
#                                               para      = 1, 
#                                               cache     = NULL,
#                                               fix_catchments = FALSE) 
# 
# write_sf(divides_old, file.path(extdata, "gage_01013500.gpkg"), "raw-divides")
# unlink(list("refactor.gpkg", "reconcile.gpkg"))




# sf::write_sf(cat_rec, "walker_cat_rec.gpkg")
# This is how the raster data was created.
# r <- fasterize::raster("NHDPlusCA/fdr.tif")
#
# cropper <- catchment %>%
#   st_transform(as.character(raster::crs(r))) %>%
#   st_union() %>%
#   st_buffer(1000) %>%
#   as_Spatial()
#
# fac <- fasterize::raster("NHDPlusCA/fac.tif")
# sub_fac <- raster::crop(fac, cropper)
# sub_r <- raster::crop(r, cropper)
# raster::writeRaster(sub_fac, "data-raw/walker_fac.tif", overwrite = TRUE)
# raster::writeRaster(sub_r, "data-raw/walker_fdr.tif", overwrite = TRUE)
# nolint end
walker_fline_ref <- sf::read_sf(file.path(extdata, "walker_refactor.gpkg"))
walker_fline_rec <- sf::read_sf(file.path(extdata, "walker_reconcile.gpkg"))
walker_catchment_rec <- sf::read_sf(file.path(extdata, "walker_cat_rec.gpkg"))