#' Add Non Network Divides to Aggregated Fabric
#' The refactoring process intentionally drop catchments without a flowpath.
#' In cases where a seamless discritization of the landscape is needed, these 
#' area must be reintroduced from the reference dataset.
#' @param gpkg a path to a gpkg
#' @param divide If gpkg is NULL, then an sf data.frame, otherwise a the layer name. See details.
#' @param huc12 huc12 to COMID crosswalk
#' @param reference_gpkg A path to the reference VPU geopackage (see get_hydrofabric(..., type = "reference"))
#' @param verbose Should messages be emitted?
#' @return gpkg path
#' @export

add_nonnetwork_divides = function(gpkg = NULL,
                                  vpu = NULL,
                                  divides = NULL,
                                  huc12 = NULL,
                                  reference_gpkg = NULL,
                                  reference_divides = NULL,
                                  verbose = TRUE){
  
  if(is.null(reference_gpkg) & is.null(reference_divides)){ stop('reference_gpkg and reference_divides cannot be NULL')}
  if(!is.null(reference_gpkg) & !is.null(reference_divides)){ stop('Either reference_gpkg or reference_divides must be NULL')}
  
  if(!is.null(reference_gpkg)){
    reference_divides = read_hydrofabric(reference_gpkg, verbose = verbose, realization = "catchments")[[1]]
    names(reference_divides) = tolower(names(reference_divides))
  } else {
    names(reference_divides) = tolower(names(reference_divides))
  }
  
  catchment_name = grep("divide|catchment", st_layers(gpkg)$name, value = TRUE)
  catchment_name = catchment_name[!grepl("network", catchment_name)]
 
  out_nl = read_hydrofabric(gpkg, 
                            catchments = catchment_name, 
                            flowpaths = NULL,
                            verbose = verbose,
                            realization = "catchments")
  
  net = read_sf(gpkg, "network") 
  
  # Encapsulated Flows
  u_fl = unique(net$hf_id)
  
  # Reference ND catchments
  non_network_divides = filter(reference_divides, !featureid %in% u_fl) %>%
    select(id = featureid) %>%
    st_transform(st_crs(out_nl$catchments)) %>%
    mutate(areasqkm = add_areasqkm(.),
           type     = ifelse(id < 0, "internal", "coastal")) %>%
    rename_geometry("geometry")
  
  cat  = left_join(out_nl$catchments, 
                   distinct(select(net, id, hydroseq)),
                   by = "id")
  
  # Coastal Catchments will be aggregated to HUC12 level POLYGONS (meaning there may be more then 1)
  # Coastal catchments do not have a ds_id as they drain to the ocean.
  coastal = filter(non_network_divides, type == 'coastal')
  
  if(nrow(coastal) > 0){
    coastal = left_join(coastal, huc12, by = "id") %>% 
      ms_dissolve("huc12") 
    
    t = st_geometry_type(coastal)
    
    if(any(t == "MULTIPOLYGON")){
      coastal = filter(coastal, t == "MULTIPOLYGON") %>% 
        ms_explode() %>% 
        bind_rows(filter(coastal, t == "POLYGON"))
    }
    
  coastal = coastal %>% 
      mutate(divide_id = 1:n(), id = NA, toid = NA, has_flowline = FALSE,
             ds_id = NA, type = "coastal",
             areasqkm = add_areasqkm(.)) %>% 
      select(divide_id, id, toid, areasqkm, type, has_flowline, ds_id) 
     
  }
  
  # Internal catchments are aggregated to their maximum polygon extent
  # a ds_id is assigned as the most downstream network divide.
  internal = filter(non_network_divides, type == 'internal') 
  
  if(nrow(internal) > 0){
    internal =  ms_explode(ms_dissolve(internal))
    
    imap = st_intersects(internal, cat)
    
    # Get ride of sinks fully contained within sinks 
    # and adjacent to network divides
    internal = filter(internal, lengths(imap) > 0)
  
    l = lengths(imap); l = l[l !=0]
    
    internal = data.frame(
      id = rep(internal$rmapshaperid, times = l),
      touch_id = out_nl$catchments$divide_id[unlist(imap)],
      touch_hydroseq = cat$hydroseq[unlist(imap)]) %>% 
      filter(id != touch_id) %>% 
      group_by(id) %>% 
      slice_min(touch_hydroseq) %>% 
      ungroup() %>% 
      left_join(rename(internal, id = rmapshaperid), by = 'id') %>% 
      st_as_sf() %>% 
      mutate(divide_id = id, id = NA, toid = NA, has_flowline = FALSE,
             ds_id = touch_id, type = "internal",
             areasqkm = add_areasqkm(.)) %>% 
      select(divide_id, id, toid, areasqkm, type, has_flowline, ds_id)
  }
  
  divides = bind_rows(internal, coastal) 
  hyaggregate_log("INFO", glue("{nrow(non_network_divides)} non network divides found"), verbose)
  
  if(nrow(divides) > 0){
    divides = divides %>% 
        mutate(divide_id = -1 * 1:n()) %>% 
        rename_geometry("geometry") %>% 
        bind_rows(rename_geometry(out_nl$catchments, "geometry"))
    
    write_sf(divides, gpkg, catchment_name, overwrite = TRUE)
    
    
    net = read_sf(gpkg, 'network')
    
    net = st_drop_geometry(divides) %>% 
      mutate(vpu = net$vpu[1], lengthkm = 0, tot_drainage_areasqkm = areasqkm) %>% 
      bind_rows(net)
    
    write_sf(net, gpkg, "network", overwrite = TRUE)
    
  }
  
  
  
  return(gpkg)

}
