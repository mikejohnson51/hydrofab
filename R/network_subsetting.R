
get_edges_terms = function(flowpaths) {
  
  fline = select(st_drop_geometry(flowpaths), id, toid)
  
  fline = mutate(fline, toid = ifelse(toid %in% id, toid, 0))

  fline
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

subset_network = function(gpkg,
                          origin,
                          flowpath_edgelist = 'flowpath_edge_list',
                          flowpath_name     = 'flowpaths',
                          catchment_name    = 'divides',
                          mainstem = FALSE,
                          attribute_layers = NULL,
                          include_ds = FALSE,
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
  
  trace = get_sorted(tmp,  outlets = origin) 
 
  trace[nrow(trace), 'toid'] = 0
  
  ids = unique(c(unlist(trace)))
  
  ll = list()
  ids_net = list()
  
  ll[['flowpaths']] = filter(read_sf(gpkg,  flowpath_name),  id %in% ids) %>% 
    select(-toid) %>% 
    left_join(trace, by = "id") 
  
  ll[['divides']]   = suppressWarnings({
    filter(read_sf(gpkg,  catchment_name), id %in% ll$flowpaths$realized_catchment) 
  })
  
  if(nrow(ll[['divides']]) == 0){
    ll[['divides']]   = filter(read_sf(gpkg,  catchment_name), id %in% ids)
  }
  
  ll$divides$toid[ll$divides$toid == tmp2$toid] = 0
  
  if ("nexus" %in% st_layers(gpkg)$name) {
    ll[['nexus']]     = filter(read_sf(gpkg,  "nexus"), id %in% ids)
  }

  if (mainstem) {
    tmp = filter(ll$flowpaths, id == origin)
    ll[['flowpaths']] = filter(ll[['flowpaths']], main_id == tmp$main_id)
  }
  
  ll$flowpath_edge_list = trace
  
  if(!is.null(attribute_layers)){
    
    ids = c(ll$divides$id, ll$flowpaths$id, ll$nexus$id)
    
    for(i in 1:length(attribute_layers)){
      if(layer_exists(gpkg, attribute_layers[i])){
        tmp = read_sf(gpkg, attribute_layers[i])
        ll[[attribute_layers[i]]] = filter(tmp, id %in% ids)
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


