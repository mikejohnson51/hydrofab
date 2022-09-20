#' Identify intersection types and downstream topology
#' @param flowpaths sf LINESTRING
#' @return data.frame with id, type, touches, touches_toID columns
#' @export
#' @importFrom nhdplusTools rename_geometry get_node
#' @importFrom sf st_intersects st_drop_geometry st_cast
#' @importFrom dplyr left_join select

define_touch_id = function(flowpaths, term_cut = 1e9){
  
  tmp =  st_cast(flowpaths, "MULTILINESTRING")
  
  ends = tmp  %>%
    nhdplusTools::rename_geometry('geometry') %>%
   
    mutate(geometry = nhdplusTools::get_node(., "end")$geometry)
  
  starts_ends = bind_rows(get_node(tmp, "start"), get_node(tmp, "end"))
  
  emap     = st_intersects(ends, starts_ends)
  tmp$type = ifelse(lengths(emap) > 1, "nex", "jun")
  tmp$type = ifelse(tmp$toid > term_cut, "term", tmp$type)
  
  ends2 = left_join(st_drop_geometry(select(ends, id)), st_drop_geometry(select(tmp, id, type)), by = "id")
  
  tmap = st_intersects(ends, tmp)
  
  data.frame(
    id            = rep(ends2$id, times = lengths(tmap)),
    type          = rep(ends2$type, times = lengths(tmap)),
    touches       = tmp$id[unlist(tmap)],
    touches_toID  = tmp$toid[unlist(tmap)]
  ) 
}



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
  
  # are fps juctions, if so who do they touch?
  touch_id <-  define_touch_id(flowpaths = network_list$flowpaths) %>% 
    filter(type == "jun") %>% 
    filter(id != touches)


  
  # bad fps are those that are both hw and too small or too large
  bad =  mutate(
    network_list$flowpaths,
    hw = ifelse(!id %in% toid, TRUE, FALSE),
    small = areasqkm < min_area_sqkm | lengthkm < min_length_km
  ) %>%
    filter(hw, small) %>%
    st_cast("MULTILINESTRING")

  # bad outlets
  outlets =  st_buffer(st_set_geometry(bad, st_geometry(get_node(bad, "end"))), 1)
  
  # intersect bad outlets with fp network
  emap = st_intersects(outlets, network_list$flowpaths)
  
  df = data.frame(
    #bad outlet ID
    id       = rep(outlets$id, times = lengths(emap)),
    #bad outlet topo toID
    toid     = rep(outlets$toid, times = lengths(emap)),
    # bad outlets touches
    touches  = network_list$flowpaths$id[unlist(emap)]
  ) %>%
    # we dont care if it touches itself
    filter(!.data$id == .data$touches) %>%
    # group_by ID
    group_by(id) %>% 
    # If the bad fp touches is topo toid, the bad id will become the min (most downstream) toid or the id it touches (most downstream)
    mutate(becomes = ifelse(any(toid == touches), min(toid), min(touches))) |>
    ungroup()   %>%
    filter(!id %in% becomes) %>% 
    select(id, becomes) %>% 
    distinct()
  
  tmp_id = filter(touch_id, touches %in% df$id) %>% 
    select(id, becomes = touches) %>% 
    group_by(becomes) %>% 
    slice_min(id) %>% 
    ungroup()

  df = df %>% 
    filter(!id %in% tmp_id$becomes) %>% 
    bind_rows(tmp_id) %>% 
    select(id, becomes) %>% 
    distinct() 
  
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
  
  #agg_net = network_list
  network_list = agg_net

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
    
    network_list  = prepare_network(network_list = list(flowpaths = fl, catchments = cat))
    
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
