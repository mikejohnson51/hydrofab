library(sf)
library(hyRefactor)

fdr <- raster::raster("~/Documents/active_code/gfv2/workspace/data/fdrfac/NHDPlusNE/NHDPlus01/NHDPlusFdrFac01a/fdr/")
fac <- raster::raster("~/Documents/active_code/gfv2/workspace/data/fdrfac/NHDPlusNE/NHDPlus01/NHDPlusFdrFac01a/fac/")
test_cat <- sf::read_sf("~/Documents/active_code/gfv2/workspace/cache/test.gpkg", "nhd_catchment")
test_fline <- sf::read_sf("~/Documents/active_code/gfv2/workspace/cache/test.gpkg", "nhd_flowline")

tf <- "~/temp/refactored.gpkg"
tr <- "~/temp/reconciled.gpkg"
refactor_nhdplus(nhdplus_flines = test_fline, 
                 split_flines_meters = 10000, 
                 split_flines_cores = 2, 
                 collapse_flines_meters = 1000,
                 collapse_flines_main_meters = 1000,
                 out_refactored = tf, 
                 out_reconciled = tr, 
                 three_pass = TRUE, 
                 purge_non_dendritic = FALSE, 
                 # exclude_cats = POIs$COMID, # Can bring this back once we don't have POIs on flowlines without catchments.
                 warn = TRUE)

test_fline_ref <- sf::read_sf("~/temp/collapsed.gpkg", "collapsed")
test_fline_rec <- sf::read_sf("~/temp/reconciled.gpkg", "reconciled")

crs <- raster::crs(fdr)

test_fline_rec <- st_transform(test_fline_rec, crs)
test_fline_ref <- st_transform(test_fline_ref, crs)
test_cat <- st_transform(test_cat, crs)

test_fline_rec <- test_fline_rec[grepl("4288455", test_fline_rec$member_COMID), ]
test_fline_ref <- test_fline_ref[test_fline_ref$COMID %in% c("4288455.1", "4288455.2", "4288455.3", "4288725", "4288717"), ]
test_cat <- test_cat[test_cat$FEATUREID %in% c(4288455, 4288725, 4288717), ]
  
reconciled_cats <- reconcile_catchment_divides(test_cat, test_fline_ref,
                                               test_fline_rec, fdr, fac)
4292649

catchment <- read_sf("data/split_cat.gpkg", "catchment")
fline <- read_sf("data/split_cat.gpkg", "fline")
fdr <- raster::raster("data/split_cat_fdr.tif")
fac <- raster::raster("data/split_cat_fac.tif")

check <- split_catchment_divide(catchment, fline, fdr, fac)

write_sf(check, "data/test.gpkg")
