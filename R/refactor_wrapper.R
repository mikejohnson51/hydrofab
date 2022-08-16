#' Refactoring Wrapper
#' @description A wrapper around hyRefactor refactor_nhdplus and
#' reconcile_catchment_divides that optionally adds routing parameters to the
#' flowpath objects.
#' @param gpkg 
#' @param flowpaths Reference flowline features
#' @param catchments Reference catchment features
#' @param events 	data.frame containing events
#' @param avoid integer vector of COMIDs to be excluded from collapse modifications.
#' @param split_flines_meters numeric the maximum length flowpath desired in the output.
#' @param collapse_flines_meters      numeric the minimum length of inter-confluence flowpath desired in the output.
#' @param collapse_flines_main_meters numeric the minimum length of between-confluence flowpaths.
#' @param cores   integer number of cores to use for parallel execution
#' @param keep    Defines the proportion of points to retain in geometry simplification (0-1; default .9). See ms_simplify.
#' @param facfdr  path to directory with flow direction and flow accumulation `SpatRast`. If NULL (default) then catchments are NOT reconciled.
#' @param routing path to National Water Model RouteLink file. If NULL (default) then routing parameters are NOT added to the refactroed flowlines.
#' @param keep proportion of points to retain in geometry simplification (0-1; default 0.05). See ms_simplify. If NULL, then no simplification will be executed.
#' @param outfile path to geopackage to write refactored_flowlines, and if facfdr != NULL, refactored catchments.
#' @return data to the specified gpkg
#' @export
#' @importFrom dplyr filter select rename
#' @importFrom hyRefactor refactor_nhdplus add_lengthmap reconcile_catchment_divides
#' @importFrom sf read_sf st_transform st_drop_geometry write_sf st_crs st_precision
#' @importFrom nhdplusTools get_streamorder get_vaa

refactor  = function (gpkg = NULL,
                      flowpaths = NULL, 
                      catchments = NULL,
                      events = NULL, avoid = NULL,
                      split_flines_meters = 10000,
                      collapse_flines_meters = 1000,
                      collapse_flines_main_meters = 1000,
                      cores = 1,
                      facfdr = NULL,
                      keep = 0.9, 
                      outfile) {
  
  
  network_list = read_hydrofabric_package(gpkg, catchments, flowpaths)
  
  tf <- tempfile(pattern = "refactored", fileext = ".gpkg")
  tr <- tempfile(pattern = "reconciled", fileext = ".gpkg")
  
  if (!is.null(events)) {
    events = dplyr::filter(events, .data$COMID %in% network_list$flowpaths$COMID)
  }
  
  if (!is.null(avoid)) {
    avoid = avoid[avoid %in% network_list$flowpaths$COMID]
  }
  
  refactor_nhdplus(nhdplus_flines = network_list$flowpaths,
                   split_flines_meters = split_flines_meters,
                   split_flines_cores = 1,
                   collapse_flines_meters = collapse_flines_meters,
                   collapse_flines_main_meters = collapse_flines_main_meters,
                   out_refactored = tf,
                   out_reconciled = tr,
                   three_pass = TRUE,
                   purge_non_dendritic = FALSE,
                   events = events,
                   exclude_cats = avoid,
                   warn = FALSE)
  
  
  rec = st_transform(read_sf(tr), 5070) %>%
    rename_geometry("geometry")
  
  if("ID.1" %in% names(rec)) {
    rec = select(rec, -"ID.1")
  }
  
  if (!is.null(facfdr) & !is.null(network_list$catchments)) {
    
    rpus         = omit.na(unique(network_list$flowpaths$RPUID))
    fdrfac_files = list.files(facfdr, pattern = rpus, full.names = TRUE)
    
    if ("featureid" %in% names(network_list$catchments)) {
      network_list$catchments = dplyr::rename( network_list$catchments, FEATUREID = .data$featureid)
    }
    
    divides <- hyRefactor::reconcile_catchment_divides(catchment = network_list$catchments,
                                                       fline_ref = sf::read_sf(tf),
                                                       fline_rec = rec,
                                                       fdr = terra::rast(grep("_fdr.tif$", fdrfac_files, value = TRUE)),
                                                       fac = terra::rast(grep("_fac.tif$", fdrfac_files, value = TRUE)),
                                                       para = cores,
                                                       cache = NULL,
                                                       keep = keep,
                                                       fix_catchments = TRUE) %>%
      rename_geometry("geometry")
  } else {
    divides = NULL
  }
  
  unlink(list(tr, tf))
  
  if(!is.null(outfile)){
    
    if(!is.null(rec)){
      write_sf(st_transform(rec, 5070),     outfile, "refactored_flowpaths",  overwrite = TRUE)
    }
    
    if(!is.null(divides)){
      write_sf(st_transform(divides, 5070), outfile, "refactored_catchments", overwrite = TRUE)
    }
    
  } else {
    list(flowpaths  = rec, catchments = divides)
  }
}