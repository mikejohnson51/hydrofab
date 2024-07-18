#' Refactoring Wrapper
#' @description A wrapper around refactor_nhdplus and reconcile_catchment_divides
#' @param gpkg a starting GPKG
#' @param flowpaths Reference flowline features
#' @param catchments Reference catchment features
#' @param events 	data.frame containing events
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

refactor  = function (gpkg = NULL,
                      flowpaths = NULL,
                      catchments = NULL,
                      pois = NULL,
                      avoid = NULL,
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
  
  fl_lookup  <- c(id = "comid", toid = "tocomid", levelpathi = "mainstemlp")
  div_lookup <- c(featureid = "divide_id", toid = "tocomid", levelpathi = "mainstemlp")
  
  network_list$flowpaths = rename(network_list$flowpaths, any_of(fl_lookup))
  
  tf <- tempfile(pattern = "refactored", fileext = ".gpkg")
  tr <- tempfile(pattern = "reconciled", fileext = ".gpkg")
  
  avoid_int <- filter(network_list$flowpaths, (sqrt(areasqkm) / lengthkm) > 3 & areasqkm > 1) 
  avoid  = c(avoid_int$id, avoid)
  avoid   = avoid[avoid %in% network_list$flowpaths$id]
  
  if(!is.null(pois)){
    events = prep_split_events(pois, network_list$flowpaths, network_list$catchments, 25) %>%
      mutate(event_identifier = as.character(row_number()))
    
    outlets <- pois %>%
      inner_join(select(st_drop_geometry(network_list$flowpaths), totdasqkm, id, dnhydroseq), 
                 by = c("hf_id" = "id"))
    
    # Need to avoid modification to flowlines immediately downstream of POIs
    #      This can cause some hydrologically-incorrect catchment aggregation
    POI_downstream <- filter(network_list$flowpaths, hydroseq %in% outlets$dnhydroseq, areasqkm > 0)
    
    ex <- unique(c(outlets$hf_id, avoid, POI_downstream$id))
  } else { 
    events = NULL
    outlets = NULL
    ex <- unique(avoid)
  }
  
  # derive list of unique terminal paths
  TerminalPaths <- unique(network_list$flowpaths$terminalpa)

  network_list$flowpaths <-
    mutate(network_list$flowpaths,
           refactor = ifelse(terminalpa %in% TerminalPaths, 1, 0)) %>% 
    rename(COMID = id,
           toCOMID = toid,
           LENGTHKM = lengthkm, 
           REACHCODE = reachcode,  
           FromMeas = frommeas,
           ToMeas   = tomeas,
           Hydroseq = hydroseq,
           LevelPathI = levelpathi,
           TotDASqKM = totdasqkm)
  
  network_list$flowpaths <-
    st_as_sf(sf::st_zm(filter(network_list$flowpaths, refactor == 1)))
  
  refactor_nhdplus(
    nhdplus_flines  = network_list$flowpaths,
    split_flines_meters         = split_flines_meters,
    split_flines_cores          = cores,
    collapse_flines_meters      = collapse_flines_meters,
    collapse_flines_main_meters = collapse_flines_main_meters,
    out_refactored              = tf,
    out_reconciled              = tr,
    three_pass                  = TRUE,
    purge_non_dendritic         = purge_non_dendritic,
    events                      = events,
    exclude_cats                = ex,
    warn                        = FALSE
  )
  
  rec =  rename_geometry(st_transform(read_sf(tr), 5070), "geometry")
  
  if ("ID.1" %in% names(rec)) { rec = select(rec, -"ID.1") }
  
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
      mutate(event_identifier = as.character(1:nrow(events))) %>%
      inner_join(st_drop_geometry(refactored_events), by = "event_identifier") %>%
      select(COMID, event_identifier, poi_id, member_COMID)

    # subset for refactored outlets (non-events)
    refactored_outlets <-
      filter(refactored, !member_COMID %in% event_outlets$member_COMID)

    # get ref_COMID for other outlets
    outlets_ref <-
      left_join(
        outlets,
        select(
          st_drop_geometry(refactored_outlets),
          member_COMID,
          orig_COMID
        ),
        by = c("hf_id" = "orig_COMID")
      ) %>%
      group_by(hf_id) %>%
      filter(member_COMID == max(member_COMID)) %>%
      select(hf_id, poi_id, member_COMID)

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
                 by = "member_COMID") %>% 
      distinct()

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

  } else {
    final_outlets = NULL
  }
  
  if (!is.null(fac) &
      !is.null(fdr) & 
      !is.null(network_list$catchments)) {
    
    div_lookup <- c(FEATUREID = "divide_id", FEATUREID = "featureid")
    
    network_list$catchments = rename(network_list$catchments, any_of(div_lookup))
    
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
    
    if (!is.null(lookup_table)) {
      write_sf(lookup_table,
               outfile,
               "lookup_table",
               overwrite = TRUE)
    }
    
    if (!is.null(final_outlets)) {
      write_sf(final_outlets,
               outfile,
               
               "outlets",
               overwrite = TRUE)
    }
    
    if (!is.null(outlets)) {
      write_sf(outlets,
               outfile,
               "pois",
               overwrite = TRUE)
    }
    
    return(outfile)
    
  } else {
    list(flowpaths  = rec, catchments = divides) 
  }
}
