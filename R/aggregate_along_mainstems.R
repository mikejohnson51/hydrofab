#' Aggregate along network mainstems
#' @description Given a set of ideal catchment sizes, plus the
#' minimum allowable catchment size and segment length, aggregate the network along mainstems.
#' @param network_list a list containing flowline and catchment `sf` objects
#' @param ideal_size The ideal size of output hydrofabric catchments
#' @param min_area_sqkm The minimum allowable size of the output hydrofabric catchments
#' @param min_length_km The minimum allowable length of the output hydrofabric flowlines
#' @param term_cut cutoff integer to define terminal IDs
#' @return a list containing aggregated and validated flowline and catchment `sf` objects
#' @export
#' @importFrom dplyr filter group_by arrange mutate ungroup select distinct
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr %>% cur_group_id n
#' @importFrom logger log_info
#' @importFrom nhdplusTools rename_geometry

aggregate_along_mainstems = function(network_list,
                                     ideal_size_sqkm,
                                     min_area_sqkm,
                                     min_length_km,
                                     verbose = TRUE,
                                     cache_file = NULL) {
  
  hyaggregate_log("INFO", "\n---  Aggregate Along Mainstem ---\n", verbose)
  hyaggregate_log("INFO", glue("ideal_size_sqkm --> {ideal_size_sqkm}"), verbose)
  hyaggregate_log("INFO", glue("min_length_km --> {min_length_km}"),     verbose)
  hyaggregate_log("INFO", glue("min_area_sqkm --> {min_area_sqkm}"),     verbose)
  
  tmp = network_list$flowpaths %>% 
    st_drop_geometry() %>% 
    select(id = toid, poi_un = poi_id) %>% 
    st_drop_geometry() %>% 
    distinct() %>% 
    filter(!is.na(poi_un)) %>% 
    group_by(id) %>% 
    slice(n = 1) %>% 
    ungroup()
  
  fline = network_list$flowpaths %>% 
    st_drop_geometry() %>% 
    mutate(poi_dn = poi_id) %>% 
    left_join(tmp, by = "id") %>% 
    distinct() 
  
  index_table = fline %>%
    group_by(.data$levelpathid) %>%
    arrange(.data$hydroseq) %>%
    mutate(poi_un = ifelse(poi_un %in% poi_dn, NA, poi_un)) %>% 
    mutate(
      ind = cs_group(
        .data$areasqkm,
        .data$lengthkm,
        .data$poi_dn,
        .data$poi_un,
        ideal_size_sqkm,
        min_area_sqkm,
        min_length_km
      )
    ) %>%
    ungroup()   %>%
    group_by(.data$levelpathid, .data$ind) %>%
    mutate(set = cur_group_id(), n = n()) %>%
    ungroup() %>%
    select(set, id, toid, levelpathid,
           hydroseq, member_comid,
           poi_id, n)
  
  v = aggregate_sets(network_list, index_table)
  
  hyaggregate_log("SUCCESS",
                  glue("Merged to idealized catchment size of {ideal_size_sqkm} sqkm: {nrow(network_list$flowpaths) - nrow(v$flowpaths)} features removed"),
                  verbose)
  
  if(!is.null(cache_file)) {
    write_hydrofabric(v,
                      cache_file,
                      "aggregate_along_mainstems_catchment",
                      "aggregate_along_mainstems_flowpath",
                      verbose)
  }
  
  return(v)
  
}

#' Cumulative sum area grouping
#' @description This function takes a vector of areas and lengths and returns a
#' index vector that combines them towards an ideal aggregate area (ideal_size_sqkm). While enforcing a minimum area (amin) and length (lmin).
#' Additionally, this function can take a set of indexes to exclude over which the network cannot be aggregated.
#' @param areas a vector of areas
#' @param lengths a vector of lengths
#' @param exclude_dn a vector of equal length to areas and lengths. Any non NA value will be used to enforce an aggregation break on the outflow node of a flowpath
#' @param exclude_un a vector of equal length to areas and lengths. Any non NA value will be used to enforce an aggregation break on the inflow node of a flowpath
#' @param ideal_size_sqkm a vector of areas
#' @param amin a threshold, or target, cumulative size
#' @param lmin a threshold, or target, cumulative size
#' @return a vector of length(areas) containing grouping indexes
#' @export
#' 

cs_group <- function(areas, lengths, exclude_dn, exclude_un, ideal_size_sqkm, amin, lmin) {
  
  areas[is.na(areas)] = 0
  lengths[is.na(lengths)] = 0
  
  if(length(areas) == 1){ return(1) }
  
  break_index = which(!is.na(exclude_dn))
  break_index2 = which(!is.na(exclude_un)) - 1
  
  break_index = sort(c(break_index, break_index2))
  
  if(length(break_index) != 0){
    sub_areas = splitAt(areas, break_index)
    sub_lengths = splitAt(lengths, break_index)
  } else {
    sub_areas = list(areas)
    sub_lengths = list(lengths)
  }
  
  if(all(lengths(sub_areas) != lengths(sub_lengths))){
    stop("Yuck~")
  }
  
  o1 = lapply(sub_areas, assign_id, athres = ideal_size_sqkm)

  o2 = lapply(1:length(sub_areas),   function(i) { pinch_sides(   x = sub_areas[[i]],   ind = o1[[i]], thres = amin) })

  o3 = lapply(1:length(sub_lengths), function(i) { pinch_sides(   x = sub_lengths[[i]], ind = o2[[i]], thres = lmin) })

  o4 = lapply(1:length(sub_areas),   function(i) { middle_massage(x = sub_areas[[i]],   ind = o3[[i]], thres = amin) })

  o5 = lapply(1:length(sub_lengths), function(i) { middle_massage(x = sub_lengths[[i]], ind = o4[[i]], thres = lmin) })
  
  for(i in 1:length(o5)){ o5[[i]] = o5[[i]] + 1e9*i }
  
  unlist(o5)
  
}

#' Re-index the edges of vector by threshold
#' Merge the outside edges of a vector if they are less then the provides threshold.
#' @param x vector of values
#' @param ind current index values
#' @param thres threshold to evaluate x
#' @return a vector of length(x) containing grouping indexes
#' @export

pinch_sides = function(x, ind, thres){
  
  tmp_areas = unlist(lapply(split(x, ind), sum))
  
  if(length(tmp_areas) == 1){ return(ind) }
  
  n = as.numeric(names(tmp_areas))
  
  if(tmp_areas[1] < thres){
    names(tmp_areas)[1] = names(tmp_areas[2])
  }
  
  if(tmp_areas[length(tmp_areas)] < thres){
    names(tmp_areas)[length(tmp_areas)] = names(tmp_areas[length(tmp_areas) - 1])
  }
  
  n2 = as.numeric(names(tmp_areas))
  
  n2[match(ind, n)]
}


#' @title Re-index the interior of vector by threshold
#' @description  Merges the interior values of a vector if they are less then the provided threshold.
#' Merging will look "up" and "down" the vector and merge into the smaller of the two.
#' @param x vector of values
#' @param index_values current index values
#' @param threshold threshold to evaluate x
#' @return a vector of length(x) containing grouping indexes
#' @export

middle_massage = function(x, index_values, threshold){
  
  tmp_areas = unlist(lapply(split(x, index_values), sum))
  
  if(length(tmp_areas) == 1){ return(index_values) }
  
  n = as.numeric(names(tmp_areas))
  
  if(any(tmp_areas < threshold)){
    tmp = which(tmp_areas < threshold)
    
    for(j in 1:length(tmp)){
      base = as.numeric(tmp[j])
      edges = c(base - 1, base + 1)
      becomes = names(which.min(tmp_areas[edges]))
      names(tmp_areas)[base] = becomes
    }
  }
  
  n2 = as.numeric(names(tmp_areas))
  
  n2[match(index_values, n)]
}




#' Enforces area and length grouping
#' @description This function takes a vector of area's and length's and returns a
#' grouping vector that enforces the grouping of lengths and areas less then defined thresholds
#' @param l a vector of lengths
#' @param a a vector of areas
#' @param lthres a minimum length that must be achieved
#' @param athres a minimum length that must be achieved
#' @return a vector of length(a) containing grouping indexes
#' @export

agg_length_area   <- function(l, a, lthres, athres) {
  
  ids = 1:length(l)
  
  if(length(ids) != 1){
    
    if(!is.null(lthres)){
      for (i in 1:(length(l)-1)) {
        if (l[i] < lthres) {
          ids[(i+1):length(l)] = ids[(i+1):length(l)] - 1
          l[i+1] = l[i] + l[i+1]
          l[i]   = l[i+1]
          a[i+1] = a[i] + a[i+1]
          a[i] =   a[i+1]
        }
      }
    }
    
    if(!is.null(athres)){
      for (i in 1:(length(a)-1)) {
        if (a[i] < athres) {
          ids[(i+1):length(a)] = ids[(i+1):length(a)] - 1
          a[i+1] = a[i] + a[i+1]
          a[i] =   a[i+1]
        }
      }
    }
    
    if(is.null(athres)){ athres = 0 }
    if(is.null(lthres)){ lthres = 0 }
    
    if(a[length(a)] < athres | l[length(l)] < lthres){
      ids[length(ids)] = pmax(1, ids[length(ids)] - 1)
    }
  }
  
  return (ids)
}



#' Split a Vector as a position
#'
#' @param x vector of values
#' @param pos split postion
#' @noRd

splitAt <- function(x, pos) {
  unname(split(x, cumsum(seq_along(x) %in% (pos + 1)) ))
}

#' Index a Vector by Cumulative Sum
#'
#' @param x a vector of values
#' @param athres the ideal cumulative size of each group
#' Cumulative sums will get as close to this value without exceeding it
#' @return a vector of length(a)
#' @export

assign_id = function(x, athres){
  
  cumsum <- 0
  group  <- 1
  result <- numeric()
  
  for (i in 1:length(x)) {
    cumsum <- cumsum + x[i]
    if (cumsum > athres) {
      group <- group + 1
      cumsum <- x[i]
    }
    result = c(result, group)
  }
  
  return (result)
}


#' Aggregate Sets by Index Table
#' @param network_list a list of flowpaths and catchments
#' @param index_table index table to aggregate with
#' @return a list of catchments and flowpaths that have been validated
#' @export
#' @importFrom dplyr group_by mutate slice_max ungroup select left_join everything filter bind_rows rename `%>%` inner_join
#' @importFrom sf st_as_sf
#' @importFrom nhdplusTools get_sorted

aggregate_sets = function(network_list, index_table) {
  
  set_topo = index_table %>%
    group_by(set) %>%
    mutate(member_comid  = paste(.data$member_comid, collapse = ","),
           poi_id  = paste(.data$poi_id[!is.na(.data$poi_id)], collapse = ","),
           poi_id  = ifelse(.data$poi_id == "", NA, .data$poi_id)) %>%
    arrange(hydroseq) %>%
    # mutate(id_new = id[1], toid_new = toid[n()]) %>%
    select(set, id, toid, levelpathid, poi_id,
           hydroseq, member_comid) %>%
    ungroup()
  
  set_topo_fin = left_join(select(set_topo, set, id = toid, hydroseq,
                                  levelpathid, poi_id, member_comid),
                           select(set_topo, toset = set, id),
                           by = "id") %>%
    group_by(set) %>%
    mutate(toset = ifelse(is.na(toset), 0, toset)) %>%
    filter(set != toset) %>%
    slice_min(hydroseq) %>%
    ungroup() %>%
    select(set, toset, levelpathid, poi_id, member_comid)
  
  ####
  
  single_flowpaths = filter(index_table, n == 1) %>%
    #chagned to inner_join from left_join!
    inner_join(network_list$flowpaths, by = "id") %>%
    st_as_sf() %>%
    select(set) %>%
    rename_geometry("geometry")
  
  flowpaths_out  = filter(index_table, n > 1) %>%
    #chagned to inner_join from left_join!
    inner_join(network_list$flowpaths, by = "id") %>%
    st_as_sf() %>%
    select(set) %>%
    union_linestrings('set') %>%
    rename_geometry("geometry") %>%
    bind_rows(single_flowpaths) %>%
    left_join(set_topo_fin, by = "set") %>%
    rename(id = set, toid = toset)
  
  ####
  
  single_catchments = filter(index_table, n == 1) %>%
    #changed to inner_join from left_join!
    inner_join(network_list$catchments, by = "id") %>%
    st_as_sf() %>%
    select(set) %>%
    rename_geometry("geometry")
  
  catchments_out  = filter(index_table, n != 1) %>%
    #changed to inner_join from left_join!
    inner_join(network_list$catchments, by = "id") %>%
    st_as_sf() %>%
    select(set) %>%
    union_polygons('set') %>%
    rename_geometry("geometry") %>%
    bind_rows(single_catchments) %>%
    left_join(set_topo_fin, by = "set") %>%
    select(id = set, toid = toset)
  
  catchments_out$toid = ifelse(is.na(catchments_out$toid), 0, catchments_out$toid)
  
  prepare_network(list(flowpaths = flowpaths_out, catchments = catchments_out))
}

