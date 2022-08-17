#' @title Build Headwater Collapse Table
#' @description  Identifies small (pathlength or area) headwater catchments and returns a data.frame
#' with the current ID and the feature ID it should collapse into (becomes).
#' Headwaters are segments in which there are no inflows (!ID %in% toID).
#' @param network_list  a list containing flowpath and catchment `sf` objects
#' @param min_area_sqkm The minimum allowable size of the output hydrofabric catchments
#' @param min_length_km The minimum allowable length of the output hydrofabric flowlines
#' @return a 2 column data.frame with {id, becomes}
#' @export
#' @importFrom dplyr mutate filter group_by mutate ungroup distinct
#' @importFrom sf st_set_geometry st_geometry st_buffer st_intersects
#' @importFrom nhdplusTools get_node

build_collapse_table = function(network_list,
                                min_area_sqkm  = 3,
                                min_length_km  = 1) {
  bad =  mutate(
    network_list$flowpaths,
    hw = ifelse(!id %in% toid, TRUE, FALSE),
    small = areasqkm < min_area_sqkm | lengthkm < min_length_km
  ) %>%
    filter(hw, small) %>%
    st_cast("MULTILINESTRING")
  
  outlets =  st_buffer(st_set_geometry(bad, st_geometry(get_node(bad, "end"))), 1)
  
  emap = st_intersects(outlets, network_list$flowpaths)
  
  df = data.frame(
    id       = rep(outlets$id, times = lengths(emap)),
    toid     = rep(outlets$toid, times = lengths(emap)),
    touches  = network_list$flowpaths$id[unlist(emap)],
    poi_id  = rep(outlets$poi_id, times = lengths(emap))
  ) %>%
    filter(!.data$id == .data$touches) %>%
    filter(is.na(poi_id)) %>%
    group_by(id) %>% 
    mutate(becomes = ifelse(any(toid == touches), toid, touches)) |>
    ungroup()   %>% 
    distinct(id, becomes) %>%
    filter(!id %in% becomes)
  
  df$mC1 = network_list$flowpaths$member_comid[match(df$id, network_list$flowpaths$id)]
  df$mC2 = network_list$flowpaths$member_comid[match(df$becomes, network_list$flowpaths$id)]
  
  group_by(df, becomes) %>%
    mutate(member_comid = paste0(mC2[1], "," ,paste(mC1, collapse = ","))) %>%
    ungroup() %>%
    select(-mC1, -mC2)
  
}

#' Collapse Headwaters
#' @description  This function identifies small (pathlength or area) headwater catchments and
#' collapses them into the existing network until none remain.
#' Headwaters are those segments in which there are no inflows (!ID %in% toID).
#' @param network_list  a list containing flowpath and catchment `sf` objects
#' @param min_area_sqkm The minimum allowable size of the output hydrofabric catchments
#' @param min_length_km The minimum allowable length of the output hydrofabric flowlines
#' @param verbose should messages be emitted?
#' @param cache_file If not NULL results will be written to a provide path (.gpkg)
#' @return a list containing flowpath and catchment `sf` objects
#' @importFrom dplyr filter left_join mutate bind_rows
#' @importFrom nhdplusTools get_node

collapse_headwaters = function(network_list,
                               min_area_sqkm  = 3,
                               min_length_km  = 1,
                               verbose = TRUE,
                               cache_file = NULL) {
  
  hyaggregate_log("INFO", "\n--- Collapse Network Inward ---\n", verbose)
  
  start <- nrow(network_list$flowpaths)
  
  mapping_table <- build_collapse_table(network_list, min_area_sqkm, min_length_km)

  count = 0
  
  while (nrow(mapping_table) > 0) {
    
    count = count + 1
    
    hyaggregate_log("INFO", glue("Collapsing: {nrow(mapping_table)} features (round {count})"), verbose)
    
    ind = match(mapping_table$becomes, network_list$flowpaths$id)
    network_list$flowpaths$member_comid[ind] = mapping_table$member_comid
    
    fl = filter(network_list$flowpaths, !id %in% mapping_table$id)
    
    cat = filter(network_list$catchments,
                 id %in% c(mapping_table$id, mapping_table$becomes)) |>
      left_join(mapping_table, by = "id") |>
      mutate(id = ifelse(is.na(becomes), id, becomes)) |>
      union_polygons("id") |>
      bind_rows(filter(
        network_list$catchments,
        !id %in% c(mapping_table$id, mapping_table$becomes)
      ))
    
    network_list = prepare_network(list(flowpaths = fl, catchments = cat))
    
    mapping_table = build_collapse_table(network_list, min_area_sqkm, min_length_km)
  }
  
  hyaggregate_log("SUCCESS", glue("Collapsed {start - nrow(network_list$flowpaths)} features."), verbose)
  
  if (!is.null(cache_file)) {
    write_hydrofabric(network_list,
                      cache_file,
                      "collapse_headwaters_catchments",
                      "collapse_headwaters_flowpaths",
                      verbose)
  }
  
  return(network_list)
}
