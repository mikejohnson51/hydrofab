build_headwater_collapse = function(network_list,
                                    min_area_sqkm  = 3,
                                    min_length_km  = 1) {
  
  touch_id <-  define_touch_id(flowpaths = network_list$flowpaths) %>% 
    filter(id != touches) %>% 
    group_by(id) %>% 
    mutate(check = ifelse(toid == touches, 1, 2)) %>% 
    slice_min(check) %>% 
    ungroup() %>% 
    select(-toid) 

  divide_touch_id = st_intersects(network_list$catchments)

  divide_touches = data.frame(
    id            = rep(  network_list$catchments$id, times = lengths(divide_touch_id)),
    becomes       =   network_list$catchments$id[unlist(divide_touch_id)]
  ) 
  
  # bad fps are those that are both hw and too small or too large
  df = left_join(network_list$flowpaths,  select(touch_id, -poi_id), by = "id") %>% 
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
  
  df$hl1 = network_list$flowpaths$poi_id[match(df$id, network_list$flowpaths$id)]
  df$hl2 = network_list$flowpaths$poi_id[match(df$becomes, network_list$flowpaths$id)]
  
  tmp = suppressWarnings({
    group_by(df, becomes) %>%
      mutate(member_comid = paste0(mC2[1], "," ,paste(mC1, collapse = ",")),
             poi_id = as.numeric(paste(unique(na.omit(c(hl1, hl2))), collapse = ","))) %>%
      ungroup() %>%
      select(-mC1, -mC2, -hl1, -hl2)
  })
  
  
  semi_join(tmp, divide_touches, by = c("id", "becomes"))
  
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
  
  network_list = add_network_type(network_list, verbose)
  
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




