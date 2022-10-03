# nolint start
library(terra)
extdata <- system.file("extdata", package = "hydrofab")

# only for rebuilding
extdata <- "inst/extdata/"

new_hope_reference <- file.path(extdata, "new_hope_reference.gpkg")
new_hope_refactor <- file.path(extdata, "new_hope_refactor.gpkg")
new_hope_agg <- file.path(extdata, "new_hope_agg.gpkg")

new_hope_collapse_temp <- file.path(extdata, "new_hope_collapse.gpkg")
new_hope_reconcile_temp <- file.path(extdata, "new_hope_reconcile.gpkg")
new_hope_cat_rec_temp <- file.path(extdata, "new_hope_cat_rec.gpkg")

fac_path <- file.path(extdata, "new_hope_fac.tif")
fdr_path <- file.path(extdata, "new_hope_fdr.tif")

new_hope_fac <- suppressWarnings(terra::rast(fac_path))
new_hope_fdr <- suppressWarnings(terra::rast(fdr_path))

proj <- as.character(terra::crs(new_hope_fdr))

nhpgpkg <- tempfile(fileext = ".gpkg")

file.copy(file.path(extdata, "new_hope.gpkg"), nhpgpkg)

nhpgpkgref <- tempfile(fileext = ".gpkg")

file.copy(file.path(extdata, "new_hope_refactor.gpkg"), nhpgpkgref)

nhpgpkgrec <- tempfile(fileext = ".gpkg")

file.copy(new_hope_reconcile_temp, nhpgpkgrec)

nhpgpkgreccat <- tempfile(fileext = ".gpkg")

file.copy(new_hope_cat_rec_temp, nhpgpkgreccat)

nhpgpkgev <- tempfile(fileext = ".gpkg")

file.copy(file.path(extdata, "new_hope_event.gpkg"), nhpgpkgev)

nhpgpkgagg <- tempfile(fileext = ".gpkg")

file.copy(new_hope_agg, nhpgpkgagg)

nhpgpkgpoi <- tempfile(fileext = ".gpkg")

file.copy(file.path(extdata, "new_hope_pois.gpkg"), nhpgpkgpoi)

new_hope_catchment <- sf::read_sf(nhpgpkg, "CatchmentSP")
new_hope_catchment <- sf::st_transform(new_hope_catchment, proj)
new_hope_flowline <- sf::read_sf(nhpgpkg, "NHDFlowline_Network")
new_hope_flowline <- sf::st_transform(new_hope_flowline, proj)
new_hope_fline_ref <- sf::read_sf(nhpgpkgref)
new_hope_fline_rec <- sf::read_sf(nhpgpkgrec)
new_hope_catchment_rec <- sf::read_sf(nhpgpkgreccat)
new_hope_events <- sf::read_sf(nhpgpkgev)
new_hope_agg_flowpath <- sf::read_sf(nhpgpkgagg, "flowpath")
new_hope_agg_divides <- sf::read_sf(nhpgpkgagg, "divides")
new_hope_pois <- sf::read_sf(nhpgpkgpoi, "mapped_POIs")

####
# This is how the raster data was created.
# library(sf)
# library(dplyr)
# source_ref <- "../gfv2/workspace/cache/reference_03N.gpkg"
# events <- sf::read_sf("../gfv2/workspace/cache/nav_03N.gpkg", "split_events")
# 
# start_COMID <- 8897784
# 
# ref_hydrofab <- sapply(st_layers(source_ref)$name, 
#                        function(x, y) {
#                          o <- read_sf(y, x)
#                          if(x != "POIs")
#                            names(o) <- tolower(names(o))
#                          o
#                         }, 
#                        y = source_ref)
# 
# ref_hydrofab$reference_flowline <- select(ref_hydrofab$reference_flowline, comid, tocomid, everything())
# 
# new_hope_subset <- nhdplusTools::get_sorted(ref_hydrofab$reference_flowline, outlets = start_COMID)
# 
# ref_hydrofab$reference_flowline <- filter(ref_hydrofab$reference_flowline, 
#                                                  comid %in% new_hope_subset$comid)
# 
# ref_hydrofab$reference_flowline <- nhdplusTools::make_standalone(ref_hydrofab$reference_flowline)
# 
# ref_hydrofab$reference_catchment <- filter(ref_hydrofab$reference_catchment, 
#                                                   featureid %in% new_hope_subset$comid)
# ref_hydrofab$reference_network <- filter(ref_hydrofab$reference_network, 
#                                                 comid %in% new_hope_subset$comid)
# 
# ref_hydrofab$reference_network <- nhdplusTools::make_standalone(ref_hydrofab$reference_network)
# 
# ref_hydrofab$lookup_table <- filter(ref_hydrofab$lookup_table,
#                                            nhdplusv2_comid %in% new_hope_subset$comid)
# ref_hydrofab$WB_03N <- filter(ref_hydrofab$WB_03N, comid %in% new_hope_subset$wbareacomi)
# ref_hydrofab$POIs <- filter(ref_hydrofab$POIs, COMID %in% new_hope_subset$comid)
# 
# invisible(lapply(names(ref_hydrofab), 
#                  function(x, y, z) {
#                    write_sf(y[[x]], z, x)
#                    NULL
#                  }, y = ref_hydrofab, z = new_hope_reference))
# 
# fac <- terra::rast("../gfv2/workspace/data/fdrfac/NHDPlusSA/NHDPlus03N/NHDPlusFdrFac03a/fac")
# fdr <- terra::rast("../gfv2/workspace/data/fdrfac/NHDPlusSA/NHDPlus03N/NHDPlusFdrFac03a/fdr")
# proj <- terra::crs(fdr)
# 
# catchment <- ref_hydrofab$reference_catchment %>%
#   st_transform(proj)
# 
# cropper <- catchment %>%
#   st_union() %>%
#   st_buffer(units::set_units(1000, "m"))
# 
# sub_fac <- terra::crop(fac, cropper)
# sub_fdr <- terra::crop(fdr, cropper)
# terra::writeRaster(sub_fac, fac_path, overwrite = TRUE)
# terra::writeRaster(sub_fdr, fdr_path, overwrite = TRUE)
# 
# sub_fac <- terra::rast(fac_path)
# sub_fdr <- terra::rast(fdr_path)
# 
# proj <- terra::crs(sub_fac)
#
# outlets_POI <- ref_hydrofab$POIs %>%
#   mutate(type = ifelse(!is.na(Type_Term), "terminal", "outlet")) 
# event_POIs <- filter(ref_hydrofab$POIs, nexus == "1") 
# 
# events <- filter(events, COMID %in% ref_hydrofab$reference_flowline$COMID)
# 
# unlink(new_hope_collapse_temp)
# unlink(new_hope_reconcile_temp)
# 
# refactor_nhdplus(nhdplus_flines = nhdplusTools::align_nhdplus_names(ref_hydrofab$reference_flowline),
#                  split_flines_meters = 2000,
#                  collapse_flines_meters = 1000,
#                  collapse_flines_main_meters = 1000,
#                  split_flines_cores = 2,
#                  out_refactored = new_hope_collapse_temp,
#                  out_reconciled = new_hope_reconcile_temp,
#                  three_pass = TRUE,
#                  events = events, 
#                  exclude_cats = c(outlets_POI$COMID),
#                  purge_non_dendritic = FALSE,
#                  warn = FALSE)
# 
# fline_ref <- read_sf(new_hope_collapse_temp) %>%
#   st_transform(proj)
# fline_rec <- read_sf(new_hope_reconcile_temp) %>%
#   st_transform(proj)
# 
# cat_rec <- reconcile_catchment_divides(nhdplusTools::align_nhdplus_names(ref_hydrofab$reference_catchment),
#                                        fline_ref, fline_rec,
#                                        fdr_path, fac_path)
# 
# cat_rec$area_sqkm <- as.numeric(st_area(
#   st_transform(cat_rec, 5070))) / (1000^2)
# 
# write_sf(cat_rec, new_hope_cat_rec_temp)
# 
# fline_rec <- inner_join(fline_rec,
#                         select(st_set_geometry(cat_rec, NULL),
#                                ID, area_sqkm), by = "ID")
# fline_rec$TotDASqKM <-
#   nhdplusTools::calculate_total_drainage_area(rename(st_set_geometry(fline_rec, NULL),
#                                                      area = area_sqkm))
# 
# write_sf(fline_rec, new_hope_reconcile_temp)
# 
# write_sf(fline_rec, new_hope_refactor, "refactored_flowpaths")
# write_sf(st_drop_geometry(fline_rec), new_hope_refactor, "catchment_network")
# write_sf(cat_rec, new_hope_refactor, "refactored_divides")
# 
# refactor_lookup <- read_sf(
#   hydrofab::generate_lookup_table(refactored_gpkg = new_hope_refactor, 
#                                                    reconciled_layer = "refactored_divides"),
#   "lookup_table")
#
## TODO: plow into code
## Join refactored to original NHD
# refactored <- read_sf(new_hope_collapse_temp)
# 
# names(refactored) <- tolower(names(refactored))
# 
# refactored <- refactored %>%
#   select(member_comid = comid, hydroseq, event_identifier, event_reachcode) %>%
#   inner_join(select(st_drop_geometry(new_hope_subset), orig_comid = comid, hydroseq), by = "hydroseq") 
# 
# # Subset for events
# refactored_events <- refactored %>%
#   filter(!is.na(event_reachcode), !is.na(event_identifier))
# 
# outlet_events <- filter(outlets_POI, nexus == "1") %>%
#   left_join(select(st_drop_geometry(refactored_events), member_comid, event_identifier, orig_comid), 
#             by = c("COMID" = "orig_comid")) %>%
#   filter(!is.na(member_comid)) %>%
#   select(-event_identifier)
# 
# # subset for refactored outlets (non-events)
# refactored_outlets <- filter(refactored, !member_comid %in% outlet_events$member_comid)
# 
# # get ref_COMId for other outlets
# outlets_ref_COMID <- filter(outlets_POI, !identifier %in% outlet_events$identifier) %>%
#   left_join(select(st_drop_geometry(refactored_outlets), member_comid, orig_comid), 
#             by = c("COMID" = "orig_comid")) %>%
#   group_by(COMID) %>%
#   filter(member_comid == max(member_comid)) %>%
#   rbind(outlet_events) %>%
#   inner_join(select(refactor_lookup, member_COMID, reconciled_ID), 
#              by = c("member_comid" = "member_COMID")) %>%
#   ungroup()
# 
# outlets <- select(st_drop_geometry(outlets_ref_COMID), ID = reconciled_ID, type)
# 
# missing_outlets <- fline_rec$ID[is.na(fline_rec$toID) &
#                                    !(fline_rec$ID %in% outlets$ID[outlets$type == "terminal"])]
# 
# if(length(missing_outlets) > 0) {
#   outlets <- bind_rows(outlets,
#                        data.frame(ID = missing_outlets,
#                                   type = rep("terminal", length(missing_outlets))))
#   
#   # not needed for new hope
#   # outlets <- outlets %>%
#   #   left_join(select(st_drop_geometry(reconciled), ID, toID), by = "ID") %>%
#   #   mutate(type = ifelse(type == "terminal" & !is.na(toID), "outlet", type)) %>%
#   #   select(-toID)
# }
# 
# aggregated <- aggregate_to_outlets(flowpath = fline_rec,
#                                    divide = cat_rec,
#                                    outlets = outlets,
#                                    da_thresh = 2, only_larger = TRUE)
# 
# aggregated$cat_sets <- pack_set(aggregated$cat_sets)
# aggregated$fline_sets <- pack_set(aggregated$fline_sets)
# 
# aggregated$cat_sets <- aggregated$cat_sets %>%
#   mutate(areasqkm = as.numeric(units::set_units(sf::st_area(.), "km^2")))
# 
# aggregated$fline_sets <- aggregated$fline_sets %>%
#   mutate(lengthkm = as.numeric(units::set_units(sf::st_length(.), "km"))) %>%
#   left_join(select(sf::st_drop_geometry(aggregated$cat_sets), ID, areasqkm), by = "ID")
# 
# write_sf(aggregated$cat_sets, new_hope_agg, "divides")
# write_sf(aggregated$fline_sets, new_hope_agg, "flowpath")
# 
# # Get physical geometry  of reconciled FL end node whose ID is an aggregated outlet
# rec_outlets <- filter(st_drop_geometry(fline_rec), ID %in% aggregated$fline_sets$ID) %>%
#   cbind(nhdplusTools::get_node(filter(fline_rec, ID %in% aggregated$fline_sets$ID))) %>%
#   st_as_sf()
# 
# # Build final POI set
# final_POIs <- st_drop_geometry(aggregated$fline_set) %>%
#   # Bring over POI information
#   left_join(st_drop_geometry(outlets_ref_COMID), by = c("ID" = "reconciled_ID")) %>%
#   # Bring over physical end node XY
#   left_join(select(rec_outlets, ID), by = "ID") %>%
#   # Mark additional POI/outlets created during aggregate cats
#   mutate(Type_Con = ifelse(!ID %in% outlets$ID, 1, 0)) %>%
#   st_as_sf()
# 
# write_sf(final_POIs, new_hope_refactor, "mapped_POIs")
# write_sf(final_POIs, new_hope_agg, "mapped_POIs")
# 
# aggregate_lookup <- read_sf(generate_lookup_table(gpkg = new_hope_agg, refactored_gpkg = new_hope_refactor), "lookup_table")
# 
# catchment_network <- read_sf(generate_catchment_network(new_hope_agg), "catchment_network")
# nolint end