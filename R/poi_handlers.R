#' Extract nexus locations for Reference POIs
#' @param gpkg a reference hydrofabric gpkg
#' @param type the type of desired POIs
#' @param verbose should messages be emitted?
#' @return data.frame with ID, type columns
#' @export
#' @importFrom sf read_sf st_drop_geometry
#' @importFrom dplyr select mutate_at vars matches filter mutate group_by summarize slice 
#' @importFrom logger log_info
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue

poi_to_outlet = function(gpkg,
                         type = c('HUC12', 'Gages', 'TE', 'NID', 'WBIn', 'WBOut'),
                         verbose = TRUE){
  
  valid_types = c('HUC12', 'Gages', 'TE', 'NID', 'WBIn', 'WBOut', "Conf", "Term", "Elev", "Travel", "Con")
  
  if(!all(type %in% valid_types)){
    bad_ids = type[!which(type %in% valid_types)]
    stop(bad_ids, " are not valid POI types. Only ", paste(valid_types, collapse = ", "), " are valid")
  }
  
  if(is.null(type)){
    type = valid_types
  }
  
  nexus_locations  = read_sf(gpkg, "mapped_POIs") %>%
    st_drop_geometry()  %>%
    select(ID, identifier, paste0("Type_", type)) %>%
    mutate_at(vars(matches("Type_")), as.character) %>%
    mutate(poi_id = as.character(identifier),
           identifier = NULL) %>%
    group_by(ID, poi_id) %>%
    ungroup() %>%
    pivot_longer(-c(poi_id, ID)) %>%
    filter(!is.na(value)) %>%
    mutate(type = gsub("Type_", "", name)) %>% 
    select(ID, poi_id, type, value) %>% 
    distinct()
  
  dups = which(duplicated(nexus_locations$ID))
  
  if(length(dups) > 0){
    
   hyaggregate_log("WARN", glue("{length(dups)} flowpaths have multiple POI IDs. One is (randomly) being selected as flowpath outlet"))
    nexus_locations =  group_by(nexus_locations, ID) %>% 
      slice(1) %>% 
      ungroup()
  
  } 
  
  
  dups2 = which(duplicated(nexus_locations$poi_id))
  
  if(length(dups2) > 0){
    
    hyaggregate_log("WARN", glue("{length(dups2)} POIs are mapped to multiple flowpaths. The most usptream will be retained"))
    nexus_locations =  group_by(nexus_locations, poi_id) %>% 
      slice(1) %>% 
      ungroup()
    
  } 
  
  hyaggregate_log("INFO", glue("{length(unique(nexus_locations$poi_id))} distinct POIs found."), verbose)
  
  nexus_locations
}




#' Add a mapped_POI layer to network_list
#'
#' @param network_list a list with flowpath and catchment data
#' @param refactored_gpkg a (optional) path to 
#' @param verbose should messages be emited?
#' @return list()
#' @export
#' @importFrom dplyr filter select mutate mutate_at vars group_by ungroup slice right_join starts_with
#' @importFrom tidyr pivot_longer 
#' @importFrom sf st_cast st_set_geometry st_geometry st_drop_geometry
#' @importFrom nhdplusTools get_node

add_mapped_pois = function(network_list, 
                           refactored_gpkg = NULL, 
                           verbose = TRUE){
  
  generate_mapped_pois = filter(network_list$flowpaths, !is.na(poi_id)) %>%
    select(id, poi_id) %>%
    st_cast("MULTILINESTRING")
  
  mapped_POIs = st_set_geometry(generate_mapped_pois,
                                                 st_geometry(get_node(generate_mapped_pois,
                                                                      position = "end" )))
  
  
  hyaggregate_log("INFO", glue("Adding {nrow(mapped_POIs)} mapped POIs to output"))
  
  if(!is.null(refactored_gpkg)){
    
    mapped_POIs = read_sf(refactored_gpkg, "mapped_POIs") %>%
      st_drop_geometry()  %>%
      select(ID, identifier, starts_with("Type_")) %>%
      mutate_at(vars(matches("Type_")), as.character) %>%
      mutate(poi_id = as.character(identifier),
             identifier = NULL) %>%
      group_by(ID, poi_id) %>%
      ungroup() %>%
      pivot_longer(-c(poi_id, ID)) %>%
      filter(!is.na(value)) %>%
      filter(poi_id %in% mapped_POIs$poi_id) %>%
      mutate(type = gsub("Type_", "", name),
             name = NULL) %>% 
      select(poi_id, value, type) %>% 
      group_by(poi_id) %>% 
      mutate(value = paste(unique(value), collapse = ","),
             type = paste(unique(type), collapse = ",")) %>% 
      slice(1) %>% 
      ungroup() %>% 
      right_join(mapped_POIs, by = "poi_id")

  }
  
  network_list$mapped_POIs = mapped_POIs
  
  return(network_list)
  
}

#' Generate Lookup table for Aggregated Network
#' @param gpkg to current network. A look up table will be added here as a layer called "lookup_table"
#' @param refactored_fabric the gpkg for the corresponding VPU preceeding gpkg
#' @return file path
#' @export
#' @importFrom sf read_sf st_drop_geometry write_sf
#' @importFrom dplyr mutate select full_join left_join
#' @importFrom tidyr unnest

generate_lookup_table = function(gpkg = NULL,
                                 refactored_gpkg = NULL) {
  
  if (is.null(gpkg) | is.null(refactored_gpkg)) {
    stop("hydrofabrics must be provided.")
  }
  
  outlets = poi_to_outlet(refactored_gpkg, verbose = FALSE) %>% 
    select(POI_ID = poi_id, POI_TYPE = type, POI_VALUE = value)
  
  lu = read_sf(refactored_gpkg, "lookup_table") %>%
    select(NHDPlusV2_COMID, reconciled_ID)
  
  nl = read_hydrofabric(gpkg)
  
  vaa = get_vaa("hydroseq", updated_network = TRUE) %>%
    rename(NHDPlusV2_COMID = comid)
  
  lu2 = nl$flowpaths %>%
    st_drop_geometry() %>%
    select(
      aggregated_ID = id,
      toID          = toid,
      member_COMID  = member_comid,
      divide_ID     = id,
      POI_ID        = poi_id,
      mainstem_id = levelpathid) %>%
    mutate(member_COMID = strsplit(member_COMID, ","),
           POI_ID = as.integer(POI_ID)) %>%
    unnest(col = 'member_COMID') %>%
    mutate(NHDPlusV2_COMID_part = sapply( strsplit(member_COMID, "[.]"), FUN = function(x){ x[2] }),
           NHDPlusV2_COMID_part = ifelse(is.na(NHDPlusV2_COMID_part), 1L, as.integer(NHDPlusV2_COMID_part)),
           NHDPlusV2_COMID = sapply( strsplit(member_COMID, "[.]"), FUN = function(x){ as.numeric(x[1]) })
    ) %>%
    full_join(lu, by = "NHDPlusV2_COMID") %>%
    left_join(vaa, by = "NHDPlusV2_COMID") %>%
    group_by(aggregated_ID) %>%
    arrange(hydroseq) %>%
    mutate(POI_ID  = as.character(c(POI_ID[1],  rep(NA, n()-1)))) %>%
    ungroup() %>%
    select(-hydroseq, - member_COMID) %>%
    left_join(outlets, by = "POI_ID") %>%
    select(NHDPlusV2_COMID, 
           NHDPlusV2_COMID_part,
           reconciled_ID,
           aggregated_ID, 
           toID, 
           mainstem = mainstem_id,
           POI_ID, 
           POI_TYPE, 
           POI_VALUE)
  
  write_sf(lu2, gpkg, "lookup_table")
  
  return(gpkg)
  
}


#add_infered_nexus