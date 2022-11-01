build_headwater_collapse = function(network_list,
                                    min_area_sqkm  = 3,
                                    min_length_km  = 1) {
  
  touch_id <-  define_touch_id(flowpaths = network_list$flowpaths) %>% 
    #filter(type == "jun") %>% 
    filter(id != touches) %>% 
    select(-toid)
  
  # bad fps are those that are both hw and too small or too large
  df = left_join(network_list$flowpaths,  touch_id, by = "id") %>% 
  mutate(
    inflow = ifelse(id %in% touch_id$touches, TRUE, FALSE),
    hw = ifelse(!id %in% toid, TRUE, FALSE),
    hw = ifelse(hw & !inflow,  TRUE, FALSE),
    small = areasqkm < min_area_sqkm | lengthkm < min_length_km
  ) %>% 
    filter(hw, small) %>% 
    st_drop_geometry() %>% 
    select(id, becomes = touches, member_comid, poi_id) %>% 
    filter(becomes != 0)
  
  df$mC1 = network_list$flowpaths$member_comid[match(df$id, network_list$flowpaths$id)]
  df$mC2 = network_list$flowpaths$member_comid[match(df$becomes, network_list$flowpaths$id)]
  
  df$poi1 = network_list$flowpaths$poi_id[match(df$id, network_list$flowpaths$id)]
  df$poi2 = network_list$flowpaths$poi_id[match(df$becomes, network_list$flowpaths$id)]
  
  suppressWarnings({
    group_by(df, becomes) %>%
      mutate(member_comid = paste0(mC2[1], "," ,paste(mC1, collapse = ",")),
             poi_id = as.numeric(paste(unique(na.omit(c(poi1, poi2))), collapse = ","))) %>%
      ungroup() %>%
      select(-mC1, -mC2, -poi1, -poi2)
  })
  
}



collapse_headwaters2 = function(network_list,
                               min_area_sqkm  = 3,
                               min_length_km  = 1,
                               verbose = TRUE,
                               cache_file = NULL) {
  
  
  hyaggregate_log("INFO", "\n--- Collapse Network Inward ---\n", verbose)
  
  start <- nrow(network_list$flowpaths)
  
  mapping_table <- build_headwater_collapse(network_list, min_area_sqkm, min_length_km)
  
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
        select(network_list$catchments, id),
        !id %in% c(mapping_table$id, mapping_table$becomes)
      )) %>% 
      left_join(st_drop_geometry(select(fl, id, toid)), by = "id")
    
    network_list  = prepare_network(network_list = list(flowpaths = fl, catchments = cat))
    
    mapping_table = build_headwater_collapse(network_list, min_area_sqkm, min_length_km)
  }
  
  hyaggregate_log("SUCCESS", glue("Collapsed {start - nrow(network_list$flowpaths)} features."), verbose)
  
  if (!is.null(cache_file)) {
    tmp = list()
    tmp$collapse_headwaters_catchments = network_list$catchments
    tmp$collapse_headwaters_flowpaths = network_list$flowpaths
    
    write_hydrofabric(tmp,
                      cache_file,
                      verbose, 
                      enforce_dm = FALSE)
    
    rm(tmp)
  }
  
  return(network_list)
}




