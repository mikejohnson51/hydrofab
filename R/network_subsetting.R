
get_edges_terms = function(flowpaths) {
  
  fline = select(st_drop_geometry(flowpaths), id, toid)

  mapview::mapview(ll$flowpaths) + ll$nexus
  
  obj2 =  data.frame(id = unique(fline$toid)) %>%
    left_join(mutate(select(fline, id), toid = id), by = "id") %>%
    mutate(toid = ifelse(is.na(.data$toid), 0, .data$toid)) %>%
    mutate(id =  paste0(
      ifelse(.data$id > term_cut, terminal_nexus_prefix, nexus_prefix),
      .data$id
    ),
    toid = paste0(catchment_prefix, .data$toid))
  
  bind_rows(obj1, obj2)
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
#' @param flowpath_edgelist layer name of flowpath edgelist in gpkg
#' @param flowpath_name layer name of flowpaths in gpkg
#' @param catchment_name layer name of cathcments in gpkg
#' @param  mainstem should only the mainstem flowpath be retruned (default = FALSE)
#' @param export_gpkg a path to write the data to. If NULL a list is returned
#' @export
#' @importFrom nhdplusTools get_sorted
#' @importFrom sf read_sf
#' @importFrom dplyr filter

# gpkg = '/Volumes/Transcend/ngen/CONUS-hydrofabric/v1.2/nextgen_01.gpkg'
# origin = "wb-12886"

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
  
  tmp = read_sf(gpkg, flowpath_edgelist)
  tmp2 = filter(tmp, id == origin)
  trace = get_sorted(tmp,  outlets = tmp2$toid)
  trace = trace[trace$id != tmp2$toid,]
  
  ll = list()
  
  ids = c(trace$id, trace$toid)
  
  ll[['flowpaths']] = filter(read_sf(gpkg,  flowpath_name),  id %in% ids)
  
  ll[['divides']]   = filter(read_sf(gpkg,  catchment_name),
                             id %in% ll$flowpaths$realized_catchment)
  
  if ("nexus" %in% st_layers(gpkg)$name) {
    ll[['nexus']]     = filter(read_sf(gpkg,  "nexus"), id %in% ll$divides$toid)
  }
  
  if (mainstem) {
    tmp = filter(ll$flowpaths, id == origin)
    ll[['flowpaths']] = filter(ll[['flowpaths']], main_id == tmp$main_id)
  }
  
  ll$flowpath_edge_list = ngen.hydrofab::get_catchment_edges_terms(ll$flowpaths, catchment_prefix = 'wb-')
  
  if(!is.null(attribute_layers)){
    for(i in 1:length(attribute_layers)){
      if(layer_exists(gpkg, attribute_layers[i])){
        tmp = read_sf(gpkg, attribute_layers[i])
        ll[[attribute_layers[i]]] = filter(tmp, id %in% ll$divides$id )
      }
    }
  }
  
  if(layer_exists(gpkg, "flowpath_attributes")){
    tmp = read_sf(gpkg, "flowpath_attributes")
    ll[["flowpath_attributes"]] = filter(tmp, id %in% ll$flowpaths$id )
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


