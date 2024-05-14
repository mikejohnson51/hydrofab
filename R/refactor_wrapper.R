omit.na = function(x) {
  x[!is.na(x)]
}

build_events = function(ref_gpkg = NULL,
                        area_length_ratio = 3,
                        area_threshold = 1,
                        flowline = "reference_flowline",
                        poi_data = 'poi_data',
                        poi_geometry_table = 'poi_geometry_table',
                        event_geometry_table = 'event_geometry_table') {
  
  fl        <- read_sf(ref_gpkg, flowline)
  POIs_data <- read_sf(ref_gpkg, poi_data)
  POIs      <- read_sf(ref_gpkg, poi_geometry_table)
  events    <- read_sf(ref_gpkg, event_geometry_table) %>%
    mutate(event_identifier = as.character(row_number()))
  
  POIs_ref <- inner_join(
    POIs,
    select(st_drop_geometry(fl), TotDASqKM, COMID, DnHydroseq),
    by = c("hy_id" = "COMID")
  )
  
  avoid <-
    filter(fl,
           (sqrt(AreaSqKM) / LENGTHKM) > area_length_ratio &
             AreaSqKM > area_threshold)
  
  # Also need to avoid modification to flowlines immediately downstream of POIs
  #      This can cause some hydrologically-incorrect catchment aggregation
  POI_downstream <-
    filter(fl, Hydroseq %in% POIs_ref$DnHydroseq, AreaSqKM > 0)
  
  # build final outlets set
  term_poi <- filter(POIs_data, hl_reference == "Type_Term")
  
  outlets <- POIs_ref %>%
    mutate(type = ifelse(poi_id %in% term_poi$poi_id, "terminal", "outlet")) %>%
    filter(!poi_id %in% events$poi_id) %>%
    rename(COMID = hy_id)
  
  events_ref <- filter(events, hy_id %in% fl$COMID) %>%
    rename(COMID = hy_id) %>%
    distinct(COMID, REACHCODE, REACH_meas, event_identifier, poi_id)
  
  list(
    events = events_ref,
    avoid =  c(outlets$COMID, avoid$COMID, POI_downstream$COMID),
    outlets = outlets
  )
  
}

#' Refactoring Wrapper
#' @description A wrapper around refactor_nhdplus and reconcile_catchment_divides
#' @param gpkg a starting GPKG
#' @param flowpaths Reference flowline features
#' @param catchments Reference catchment features
#' @param events 	data.frame containing events
#' @param outlets DO I NEED THIS?
#' @param avoid integer vector of COMIDs to be excluded from collapse modifications.
#' @param split_flines_meters numeric the maximum length flowpath desired in the output.
#' @param collapse_flines_meters      numeric the minimum length of inter-confluence flowpath desired in the output.
#' @param collapse_flines_main_meters numeric the minimum length of between-confluence flowpaths.
#' @param cores   integer number of cores to use for parallel execution
#' @param keep    Defines the proportion of points to retain in geometry simplification (0-1; default .9). See ms_simplify.
#' @param fac  path to flow accumulation grid. If NULL (default) then catchments are NOT reconciled.
#' @param fdr  path to flow direction grid. If NULL (default) then catchments are NOT reconciled.
#' @param keep proportion of points to retain in geometry simplification (0-1; default 0.05). See ms_simplify. If NULL, then no simplification will be executed.
#' @param outfile path to geopackage to write refactored_flowlines, and if facfdr != NULL, refactored catchments.
#' @return data to the specified gpkg
#' @export
#' @importFrom dplyr filter select rename
#' @importFrom sf read_sf st_transform st_drop_geometry write_sf st_crs st_precision
#' @importFrom nhdplusTools get_streamorder get_vaa


refactor  = function (gpkg = NULL,
                      flowpaths = NULL,
                      catchments = NULL,
                      events = NULL,
                      avoid = NULL,
                      outlets = NULL,
                      split_flines_meters = 10000,
                      collapse_flines_meters = 1000,
                      collapse_flines_main_meters = 1000,
                      cores = 1,
                      fac = NULL,
                      fdr = NULL,
                      purge_non_dendritic = TRUE,
                      keep = NULL,
                      outfile = NULL) {
  
  network_list = read_hydrofabric(gpkg, catchments, flowpaths)
  
  tf <- tempfile(pattern = "refactored", fileext = ".gpkg")
  tr <- tempfile(pattern = "reconciled", fileext = ".gpkg")
  
  if (!is.null(events)) {
    events  = filter(events, COMID %in% network_list$flowpaths$COMID)
  }
  
  if (!is.null(avoid))  {
    avoid   = avoid[avoid %in% network_list$flowpaths$COMID]
  }
  
  if (!is.null(outlets)) {
    outlets = filter(outlets, COMID %in% network_list$flowpaths$COMID)
  }
  
  # derive list of unique terminal paths
  TerminalPaths <- unique(network_list$flowpaths$terminalpa)
  
  network_list$flowpaths <-
    mutate(network_list$flowpaths,
           refactor = ifelse(terminalpa %in% TerminalPaths, 1, 0)) %>% 
    rename(COMID = comid,
           toCOMID = tocomid,
           LENGTHKM = lengthkm, 
           Hydroseq = hydroseq,
           LevelPathI = levelpathi,
           TotDASqKM = totdasqkm)
  
  network_list$flowpaths <-
    st_as_sf(sf::st_zm(filter(network_list$flowpaths, refactor == 1)))
  
  # nhdplus_flines  = select(network_list$flowpaths, -ftype)
  # split_flines_meters         = split_flines_meters
  # split_flines_cores          = cores
  # collapse_flines_meters      = collapse_flines_meters
  # collapse_flines_main_meters = collapse_flines_main_meters
  # out_refactored              = tf
  # out_reconciled              = tr
  # three_pass                  = TRUE
  # purge_non_dendritic         = purge_non_dendritic
  # events                      = events
  # exclude_cats                = avoid
  # warn                        = FALSE
  
  refactor_nhdplus(
    nhdplus_flines  = select(network_list$flowpaths, -ftype),
    split_flines_meters         = split_flines_meters,
    split_flines_cores          = cores,
    collapse_flines_meters      = collapse_flines_meters,
    collapse_flines_main_meters = collapse_flines_main_meters,
    out_refactored              = tf,
    out_reconciled              = tr,
    three_pass                  = TRUE,
    purge_non_dendritic         = purge_non_dendritic,
    events                      = events,
    exclude_cats                = avoid,
    warn                        = FALSE
  )
  
  rec =  rename_geometry(st_transform(read_sf(tr), 5070), "geometry")
  
  if ("ID.1" %in% names(rec)) {
    rec = select(rec, -"ID.1")
  }
  
  ##### LOOKUP #####
  refactor_lookup <- st_drop_geometry(rec) %>%
    select(ID, member_COMID) %>%
    mutate(member_COMID = strsplit(member_COMID, ",")) %>%
    unnest(cols = member_COMID) %>%
    mutate(NHDPlusV2_COMID = as.numeric(member_COMID)) %>%
    rename(reconciled_ID = ID)
  
  if (is.character(refactor_lookup$reconciled_ID)) {
    refactor_lookup$reconciled_ID <-
      as.numeric(refactor_lookup$reconciled_ID)
  }
  
  lookup_table <-
    data.frame(NHDPlusV2_COMID = unique(as.numeric(refactor_lookup$member_COMID))) %>%
    left_join(refactor_lookup, by = "NHDPlusV2_COMID")
  
  # Join refactored to original NHD
  refactored <- read_sf(tf) %>%
    select(member_COMID = COMID, Hydroseq, event_identifier, event_REACHCODE) %>%
    inner_join(select(
      st_drop_geometry(network_list$flowpaths),
      orig_COMID = COMID,
      Hydroseq
    ), by = "Hydroseq")
  
  if (!is.null(events)) {
    # Subset for events
    refactored_events <- refactored %>%
      filter(!is.na(event_REACHCODE), !is.na(event_identifier))
    
    event_outlets <- events %>%
      inner_join(st_drop_geometry(refactored_events), by = "event_identifier") %>%
      select(COMID, event_identifier, poi_id, member_COMID)
    
    # subset for refactored outlets (non-events)
    refactored_outlets <-
      filter(refactored, !member_COMID %in% event_outlets$member_COMID)
    
    # get ref_COMId for other outlets
    outlets_ref <- outlets %>%
      left_join(
        select(
          st_drop_geometry(refactored_outlets),
          member_COMID,
          orig_COMID
        ),
        by = c("COMID" = "orig_COMID")
      ) %>%
      group_by(COMID) %>%
      filter(member_COMID == max(member_COMID)) %>%
      select(hy_id = COMID, poi_id, member_COMID, type)
    
    outlets_ref_COMID <-
      data.table::rbindlist(list(outlets_ref, event_outlets), fill = TRUE) %>%
      st_as_sf()
  } else {
    if (!is.null(outlets)) {
      outlets_ref_COMID <- outlets %>%
        left_join(
          select(st_drop_geometry(refactored), member_COMID, orig_COMID),
          by = c("COMID" = "orig_COMID")
        ) %>%
        group_by(COMID) %>%
        filter(member_COMID == max(member_COMID)) %>%
        select(hy_id = COMID, poi_id, member_COMID, type)
    } else {
      outlets_ref_COMID = NULL
    }
  }
  
  if (!is.null(outlets_ref_COMID)) {
    final_outlets <-  outlets_ref_COMID %>%
      st_as_sf() %>%
      inner_join(select(lookup_table, member_COMID, reconciled_ID),
                 by = "member_COMID")
    
    check_dups_poi <- final_outlets %>%
      group_by(reconciled_ID) %>%
      filter(n() > 1) %>%
      ungroup()
    
    if (nrow(check_dups_poi) > 1) {
      hydrofab::hyaggregate_log("WARN", "Double-check for double POIs")
    } else {
      hydrofab::hyaggregate_log("SUCCESS", "No double POIs detected")
    }
    
    hydrofab::hyaggregate_log("SUCCESS", "Flowlines successfully refactored.")
    
  }
  
  
  if (!is.null(fac) &
      !is.null(fdr) & 
      !is.null(network_list$catchments)) {

    if ("featureid" %in% names(network_list$catchments)) {
      network_list$catchments = dplyr::rename(network_list$catchments, FEATUREID = FEATUREID)
    }
    
    fac_open = climateR::dap(URL = fac, AOI = network_list$catchments)
    fdr_open = climateR::dap(URL = fdr, AOI = network_list$catchments)
    
    divides <-
      reconcile_catchment_divides(
        catchment = network_list$catchments,
        fline_ref = read_sf(tf),
        fline_rec = rec,
        fdr = fdr_open,
        fac = fac_open,
        para = cores,
        cache = NULL,
        keep = keep,
        fix_catchments = FALSE
      ) %>%
      rename_geometry("geometry")
  } else {
    divides = NULL
  }
  
  unlink(list(tr, tf))
  
  ## Write Mapped POIs
  # TODO:
  ##
  
  if (!is.null(outfile)) {
    if (!is.null(rec)) {
      write_sf(st_transform(rec, 5070),
               outfile,
               "refactored_flowpaths",
               overwrite = TRUE)
    }
    
    if (!is.null(divides)) {
      write_sf(st_transform(divides, 5070),
               outfile,
               "refactored_catchments",
               overwrite = TRUE)
    }
    
    return(outfile)
    
  } else {
    list(flowpaths  = rec, catchments = divides)
  }
}
