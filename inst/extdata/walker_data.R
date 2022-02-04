# nolint start
suppressWarnings(library(terra))
extdata <- system.file("extdata", package = "hyRefactor")
walker_fac <- suppressWarnings(terra::rast(file.path(extdata, "walker_fac.tif")))
walker_fdr <- suppressWarnings(terra::rast(file.path(extdata, "walker_fdr.tif")))

wgpkg <- tempfile(fileext = ".gpkg")

file.copy(file.path(extdata, "walker.gpkg"), wgpkg)

walker_catchment <- sf::read_sf(wgpkg, "CatchmentSP")
walker_catchment <- sf::st_transform(walker_catchment, terra::crs(walker_fdr))
walker_flowline  <- sf::read_sf(wgpkg, "NHDFlowline_Network")
walker_flowline  <- sf::st_transform(walker_flowline, terra::crs(walker_fdr))

# walker.gpkg turned into pre-processed sample data.
# run the above then:
# refactor <- refactor_nhdplus(nhdplus_flines = walker_flowline,
#                              split_flines_meters = 2000,
#                              collapse_flines_meters = 1,
#                              collapse_flines_main_meters = 1,
#                              split_flines_cores = 2,
#                              out_refactored = "walker_refactor.gpkg",
#                              out_reconciled = "walker_reconcile.gpkg",
#                              three_pass = TRUE,
#                              purge_non_dendritic = FALSE,
#                              warn = FALSE)
# fline_ref <- sf::read_sf("walker_refactor.gpkg")
# fline_rec <- sf::read_sf("walker_reconcile.gpkg")
# cat_rec <- reconcile_catchment_divides(walker_catchment, fline_ref,
#                                 fline_rec, walker_fdr, walker_fac)
# sf::write_sf(cat_rec, "walker_cat_rec.gpkg")
# This is how the raster data was created.
# r <- fasterize::raster("NHDPlusCA/fdr.tif")
#
# cropper <- catchment %>%
#   st_transform(terra::crs(r)) %>%
#   st_union() %>%
#   st_buffer(1000) %>%
#   as_Spatial()
#
# fac <- fasterize::raster("NHDPlusCA/fac.tif")
# sub_fac <- terra::crop(fac, cropper)
# sub_r <- terra::crop(r, cropper)
# terra::writeRaster(sub_fac, "data-raw/walker_fac.tif", overwrite = TRUE)
# terra::writeRaster(sub_r, "data-raw/walker_fdr.tif", overwrite = TRUE)
# nolint end
walker_fline_ref     <- sf::read_sf(file.path(extdata, "walker_refactor.gpkg"))
walker_fline_rec     <- sf::read_sf(file.path(extdata, "walker_reconcile.gpkg"))
walker_catchment_rec <- sf::read_sf(file.path(extdata, "walker_cat_rec.gpkg"))
