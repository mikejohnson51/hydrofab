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
    mutate(type = gsub("Type_", "", name),
           name = NULL) %>% 
    select(ID, poi_id) %>% 
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




add_mapped_pois = function(network_list, refactored_gpkg = NULL, verbose = TRUE){
  
  generate_mapped_pois = filter(network_list$flowpaths, !is.na(poi_id)) %>%
    select(id, poi_id) %>%
    st_cast("MULTILINESTRING")
  
  mapped_POIs = st_set_geometry(generate_mapped_pois,
                                                 st_geometry(get_node(generate_mapped_pois,
                                                                      position = "end" )))
  
  
  hyaggregate_log("INFO", glue("Adding {nrow(mapped_POIs)} mapped POIs to output"))
  
  if(!is.null(refactored_gpkg)){
    
    mapped_POIs = read_sf(gpkg, "mapped_POIs") %>%
      st_drop_geometry()  %>%
      select(ID, identifier, paste0("Type_", type)) %>%
      mutate_at(vars(matches("Type_")), as.character) %>%
      mutate(poi_id = as.character(identifier),
             identifier = NULL) %>%
      group_by(ID, poi_id) %>%
      ungroup() %>%
      pivot_longer(-c(poi_id, ID)) %>%
      filter(!is.na(value)) %>%
      mutate(type = gsub("Type_", "", name),
             name = NULL) %>% 
      select(poi_id, value, type) %>% 
      group_by(poi_id) %>% 
      mutate(value = paste(value, sep = ","),
             type = paste(type, sep = ",")) %>% 
      slice(1) %>% 
      ungroup() %>% 
      right_join(mapped_POIs, by = "poi_id")

  }
  
  network_list$mapped_POIs = mapped_POIs
  
  return(network_list)
  
}

#add_infered_nexus