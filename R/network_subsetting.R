
get_edges_terms = function(flowpaths) {
  
  fline = select(st_drop_geometry(flowpaths), id, toid)
  
  fline = mutate(fline, toid = ifelse(toid %in% id, toid, 0))

  fline
}

extract_prefix = function(input, col) {
  
  for (i in col) {
    input[[i]] = gsub("(-).*", "\\1", input[[i]])
  }
  input
}


#' Find ID from location
#' @param gpkg path to a hydrofabric
#' @param pt a spatial point (sf)
#' @return a waterbody ID (character)
#' @export
#' @importFrom sf read_sf st_transform

find_origin = function(gpkg, pt, catchment_name = "divides") {
  tmp = read_sf(gpkg,  catchment_name)[st_transform(pt, 5070), ]
  gsub("cat-", "wb-", tmp$id)
}

#' Subset the upstream portion of a network
#' @param gpkg path to a hydrofabric
#' @param origin the ID to begin navigation
#' @param flowpath_edgelist layer name of flowpath edge list in gpkg
#' @param flowpath_name layer name of flowpaths in gpkg
#' @param catchment_name layer name of catchments in gpkg
#' @param mainstem should only the mainstem flowpath be returned (default = FALSE)
#' @param attribute_layers layer name of additional tables to be subset
#' @param include_ds should the feature downstream of the origin be included (default = FALSE)
#' @param export_gpkg a path to write the data to. If NULL a list is returned
#' @export
#' @importFrom nhdplusTools get_sorted
#' @importFrom sf read_sf
#' @importFrom dplyr filter
#' 
# gpkg             = '/Volumes/Transcend/ngen/CONUS-hydrofabric/calibration/uniform_01.gpkg'
# 
# c  = read_sf(gpkg, "lookup_table") %>%
#   filter(POI_TYPE == "Gages") %>%
#   filter(POI_VALUE %in% "01073000")
# 
# yy = subset_network(gpkg,
#                origin = c$aggregated_ID)
# 
# mapview::mapview(yy$divides) + yy$flowpaths
# # # 
# gpkg             = '/Volumes/Transcend/ngen/CONUS-hydrofabric/calibration/nextgen_01.gpkg'
# 
# c  = read_sf(gpkg, "lookup_table") %>%
#   filter(POI_TYPE == "Gages") %>%
#   filter(POI_VALUE %in% "01073000")
# 
# xx = subset_network(gpkg,  origin = c$toid)
# 
# mapview::mapview(xx$divides) + xx$flowpaths + xx$nexus


subset_network = function(gpkg,
                          origin,
                          flowpath_edgelist = 'flowpath_edge_list',
                          flowpath_name     = 'flowpaths',
                          catchment_name    = 'divides',
                          mainstem = FALSE,
                          attribute_layers = NULL,
                          export_gpkg = NULL,
                          overwrite = FALSE,
                          verbose = TRUE) {
  

  
  if(!is.null(export_gpkg)){
    if(file.exists(export_gpkg) & !overwrite){
      return(export_gpkg)
    }
  }
  
  tmp = read_sf(gpkg, flowpath_edgelist) %>% 
    select(id, toid)
  
  wb_prefix = extract_prefix(tmp, 'id')$id[1]
  
  if(wb_prefix == as.character(tmp$id[1])){
    terminal = 0
  } else {
    terminal = paste0(wb_prefix, 0)
  }
  
  tmp2 = filter(tmp, id == origin) 
  
  trace = get_sorted(tmp,  outlets = origin) 
 
  if(!is.null(terminal)){
    trace[nrow(trace), 'toid'] = terminal
  } else {
    trace =  trace[-nrow(trace),]
  }
 
  ids = unique(c(unlist(trace)))
  
  ll = list()
  
  ll[['flowpaths']] = filter(read_sf(gpkg,  flowpath_name),  id %in% ids) %>% 
    select(-toid) %>% 
    left_join(trace, by = "id") %>% 
    mutate(toid = ifelse(is.na(toid), 0, toid))
  
  ll[['divides']]   = suppressWarnings({
    filter(read_sf(gpkg,  catchment_name), id %in% ll$flowpaths$realized_catchment) 
  })
  
  
  if(nrow(ll[['divides']]) == 0){
    ll[['divides']]   = filter(read_sf(gpkg,  catchment_name), id %in% ids)
  }

  #mapview::mapview(ll$flowpaths) + ll$divides

  if(!is.null(terminal)){
    ll$divides$toid[ll$divides$toid == tmp2$toid] = terminal
  }
  
  if (layer_exists(gpkg, "nexus")) {
    ll[['nexus']]     = filter(read_sf(gpkg,  "nexus"), id %in% ids) %>% 
      mutate(toid = NULL) %>% 
      left_join(trace, by = "id")   %>% 
      mutate(toid = ifelse(is.na(toid), 0, toid))
  }
  
  if (layer_exists(gpkg, "lookup_table")) {
    #TODO: GROSS!!!!
    ll[['lookup_table']]     = tryCatch({
      filter(read_sf(gpkg,  "lookup_table"), id %in% ids) }, 
      error = function(e){
        filter(read_sf(gpkg,  "lookup_table"), aggregated_ID %in% ids) }
      )
  }

  if (mainstem) {
    tmp = filter(ll$flowpaths, id == origin)
    ll[['flowpaths']] = filter(ll[['flowpaths']], main_id == tmp$main_id)
  }
  
  ll$flowpath_edge_list = trace
  
  if(!is.null(attribute_layers)){
    
    all_ids = c(ll$divides$id, ll$flowpaths$id, ll$nexus$id)
    
    for(i in 1:length(attribute_layers)){
      if(layer_exists(gpkg, attribute_layers[i])){
        tmp = read_sf(gpkg, attribute_layers[i])
        ll[[attribute_layers[i]]] = filter(tmp, id %in% all_ids)
      }
    }
  }
  
  if (!is.null(export_gpkg)) {
    if (length(ll) > 0) {
      names = names(ll)
      
      for (i in 1:length(ll)) {
        hyaggregate_log("INFO", glue("Writing  {names[i]}"), verbose)
        write_sf(ll[[i]], export_gpkg, names[i])
      }
    }
    
    return(export_gpkg)
  } else {
    return(ll)
  }
}


