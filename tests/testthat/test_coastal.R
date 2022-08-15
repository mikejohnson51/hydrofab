context("coastal aggregation")

# bbox <- sf::st_bbox(c(xmin = -124.383377, ymin = 39.765797, xmax = -123.705239, ymax = 40.377652), crs = sf::st_crs(4326))
# plot_nhdplus(bbox = bbox, gpkg = "tests/testthat/data/coastal.gpkg", overwrite = TRUE)

source_gpkg <- list.files(pattern = "coastal.gpkg$", recursive = TRUE, full.names = TRUE)

# nhd <- read_sf(source_gpkg, "NHDFlowline_Network") %>%
#   align_nhdplus_names()
# 
# terminals <- filter(nhd, TerminalFl == 1)
# 
# coastal <- filter(nhd, FTYPE == "Coastline")
# 
# fl <- lapply(terminals$COMID, function(x) {
#   navigate_nldi(list(featureSource = "comid", featureID = x), mode = "UT")
# })
# 
# flowline <- bind_rows(fl)
# 
# sub <- subset_nhdplus(c(flowline$nhdplus_comid, coastal$COMID), output_file = source_gpkg, overwrite = TRUE,
#                       nhdplus_data = "download")
# 
# nhd <- read_sf(source_gpkg, "NHDFlowline_Network") %>%
#   align_nhdplus_names()
# 
# cats <- read_sf(source_gpkg, "CatchmentSP") %>%
#   align_nhdplus_names()
# 
# cats <- cats[cats$FEATUREID %in% nhd$COMID, ]
# 
# nhd <- read_sf(source_gpkg, "NHDFlowline_Network") %>%
#   align_nhdplus_names()

source_fdr <- list.files(pattern = "coastal_fdr.tif$", recursive = TRUE, full.names = TRUE)
source_fac <- list.files(pattern = "coastal_fac.tif$", recursive = TRUE, full.names = TRUE)
min_da_km <- 10

# fdr <- terra::rast("~/Documents/active_code/gfv2/workspace/data/fdrfac/NHDPlusCA/NHDPlus18/NHDPlusFdrFac18c/fdr/")
# fac <- terra::rast("~/Documents/active_code/gfv2/workspace/data/fdrfac/NHDPlusCA/NHDPlus18/NHDPlusFdrFac18c/fac/")
# crs <- terra::crs(fac)
# 
# fdr <- terra::crop(fdr, vect(st_transform(cats, crs)))
# fac <- terra::crop(fac, vect(st_transform(cats, crs)))
# 
# terra::writeRaster(fdr, "tests/testthat/data/coastal_fdr.tif")
# terra::writeRaster(fac, "tests/testthat/data/coastal_fac.tif")

test_that("basic coastal aggregation", {
  nhd <- sf::read_sf(source_gpkg, "NHDFlowline_Network") %>%
    nhdplusTools::align_nhdplus_names()
  
  coastal <- nhd[nhd$FTYPE == "Coastline", ]
  
  nhd <- dplyr::filter(nhd, COMID %in% nhdplusTools::prepare_nhdplus(nhd, 0, 0, 0, FALSE, skip_toCOMID = TRUE, warn = FALSE)$COMID)
  
  networks <- do.call(c, sapply(dplyr::filter(nhd, TerminalFl == 1)$COMID, 
                                function(x, nhd) nhdplusTools::get_UT(network = nhd, x), nhd = nhd))
  
  nhd <- dplyr::filter(nhd, COMID %in% networks)
  
  nhd <- nhdplusTools::make_standalone(nhd)
  
  nhd_outlets <- dplyr::filter(nhd, TerminalFl == 1 | 
                                 !ToNode %in% FromNode) %>%
    sf::st_sf()
  
  cats <- sf::read_sf(source_gpkg, "CatchmentSP") %>%
    nhdplusTools::align_nhdplus_names()
  
  coastal_cats <- cats[cats$FEATUREID %in% coastal$COMID, ]
  
  cats <- cats[cats$FEATUREID %in% nhd$COMID, ]

  sf::st_geometry(nhd_outlets) <- sf::st_geometry(nhdplusTools::get_node(nhd_outlets, "end"))
  
  little_terminal <- dplyr::filter(nhd, TerminalPa %in% 
                              dplyr::filter(nhd_outlets, 
                                     TotDASqKM <= min_da_km & 
                                       TerminalFl == 1)$TerminalPa)
  
  outlets <- dplyr::select(nhd_outlets, COMID) %>%
    dplyr::mutate(type = "terminal") %>%
    dplyr::filter(COMID %in% cats$FEATUREID) %>%
    dplyr::mutate(keep = ifelse(COMID %in% little_terminal$COMID, "temporary", "keep"))
  
  avoid <- dplyr::filter(nhd, (sqrt(AreaSqKM) / LENGTHKM) > 3 & AreaSqKM > 1)
  
  tf <- file.path(tempfile(fileext = ".gpkg"))
  tr <- file.path(tempfile(fileext = ".gpkg"))
  
  refactor_nhdplus(nhdplus_flines = nhd, 
                   split_flines_meters = 100000, 
                   split_flines_cores = 1, 
                   collapse_flines_meters = 2000,
                   collapse_flines_main_meters = 2000,
                   out_refactored = tf, 
                   out_reconciled = tr, 
                   three_pass = TRUE, 
                   purge_non_dendritic = FALSE, 
                   exclude_cats = unique(c(avoid$COMID, outlets$COMID, little_terminal$COMID)),
                   warn = FALSE)
  
  fdr <- suppressWarnings(terra::rast(source_fdr))
  fac <- suppressWarnings(terra::rast(source_fac))
  crs <- terra::crs(fac)
  
  refactored <- sf::st_transform(sf::read_sf(tf), crs)
  reconciled <- sf::st_transform(sf::read_sf(tr), crs)
  cats <- sf::st_transform(cats, crs)
  sf::st_precision(cats) <- 10
  
  divides <- reconcile_catchment_divides(catchment = cats,
                                         fline_ref = refactored,
                                         fline_rec = reconciled,
                                         fdr = fdr,
                                         fac = fac,
                                         para = 1) 
  
  keep_outlets <- dplyr::filter(outlets, keep == "keep") %>%
    dplyr::select(COMID, type)
  
  mapped_outlets <- map_outlet_ids(keep_outlets, reconciled) %>%
    dplyr::filter(COMID %in% keep_outlets$COMID)
  
  zero_order <- list(basin = little_terminal$COMID, zero = coastal$COMID)
  
  agg_cats <- aggregate_catchments(flowpath = reconciled, 
                                   divide = divides, 
                                   outlets = dplyr::select(mapped_outlets, ID, type),
                                   zero_order = zero_order,
                                   coastal_cats = sf::st_transform(coastal_cats, sf::st_crs(divides)),
                                   da_thresh = 1, 
                                   only_larger = TRUE)
  
  expect_equal(names(agg_cats)[3], "coastal_sets")
  
  expect_equal(names(agg_cats$coastal_sets), c("ID", "geom"))
  
  expect_equal(nrow(agg_cats$coastal_sets), 2)
})
