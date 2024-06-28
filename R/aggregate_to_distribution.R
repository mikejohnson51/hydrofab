#' @title Aggregate Network to Uniform Size
#' @description This function aggregates a network to a desired size distribution while
#' enforcing minimum flowpath legnths and catchment areas. Additionally a set of explicit nexus
#' locations can be provided over which the network cannot be aggregated (see poi_to_outlet)
#' @param gpkg a path to a gpkg
#' @param divide If gpkg is NULL, then an sf data.frame, otherwise a the layer name. See details.
#' @param flowpath If gpkg is NULL, then an sf data.frame, otherwise a the layer name. See details.
#' @param outlets data.frame with mandatory "ID" column and optional "POI_ID" column. "ID" must be identifiers from
#' flowpath and divide data.frames and POI ID must be unique.
#' @param ideal_size_sqkm The ideal size of catchments (default = 10 sqkm)
#' @param min_length_km The minimum allowable length of flowpath features (default = 1 km)
#' @param min_area_sqkm The minimum allowable area of catchment features (default = 3 sqkm)
#' @param outfile of not NULL, where to write the output files
#' @param overwrite overwrite existing gf file. Default is FALSE
#' @param nexus_locations a data.frame with columns specifying the ID, and the nexus type.
#' @param log a filepath to write messages to or Boolean (TRUE = print to console; FALSE = no messages)
#' @param verbose print status updates. Default = TRUE
#' @return if outfile = TRUE, a file path, else a list object
#' @details If gpkg is not NULL, divide and flowpath can be left NULL as well. The code attempts to
#' infer the correct layers. The divides layer will be the one including the word "divide" or "catchment" and the
#' flowpath layer will be the one including 'flowpath' or 'flowline'. If no layers, or more then one layer are deemed possible
#' for each input, then the function will stop and ask for explicit names.
#' @export
#' @importFrom sf st_transform read_sf st_set_crs write_sf st_layers
#' @importFrom dplyr left_join filter semi_join
#' @importFrom nhdplusTools get_sorted calculate_total_drainage_area get_streamorder
#' @importFrom logger log_appender appender_file appender_console
#' @importFrom tidyr separate_longer_delim

aggregate_to_distribution = function(gpkg = NULL,
                                     vpu  = NULL,
                                     flowpath = NULL,
                                     divide = NULL,
                                     hydrolocations = NULL,
                                     ideal_size_sqkm = 10,
                                     min_length_km = 1,
                                     min_area_sqkm  = 3,
                                     outfile = NULL,
                                     log = TRUE,
                                     overwrite = FALSE,
                                     cache = FALSE,
                                     verbose = TRUE) {
  
  if (cache &  is.null(outfile)) {
    stop("cache cannot be written if outfile is NULL")
  }
  
  if (cache) {
    cache_file = outfile
  } else {
    cache_file = NULL
  }
  
  if (!is.logical(log)) {
    log_appender(appender_file(log))
    verbose = TRUE
  } else {
    log_appender(appender_console)
    verbose = log
  }
  
  if (!is.null(outfile)) {
    if (file.exists(outfile) & overwrite) {
      unlink(outfile)
    } else if (file.exists(outfile)) {
      hyaggregate_log("WARN",
                      glue("{outfile} already exists and overwrite is FALSE"),
                      verbose)
      return(outfile)
    }
  }
  
  network_list <- read_hydrofabric(gpkg,
                                   catchments = divide,
                                   flowpaths = flowpath,
                                   crs = 5070) |> 
    prepare_network() |> 
    add_network_type(verbose = FALSE)
  
  # Add outlets
  if (!is.null(hydrolocations)) {
    
    names(hydrolocations) = tolower(names(hydrolocations))
    
    outflows = hydrolocations %>%
      st_drop_geometry() %>% 
      select(poi_id, id) %>% 
      filter(!is.na(poi_id)) |> 
      group_by(id) %>% 
      mutate(poi_id = paste(na.omit(poi_id), collapse = ",")) %>% 
      slice(1) %>% 
      ungroup()

    network_list$flowpaths  = left_join(mutate(network_list$flowpaths, poi_id = NULL), 
                                        st_drop_geometry(outflows), 
                                        by = 'id')
  } else {
    network_list$flowpaths$poi_id    = NA
    network_list$flowpaths$hl_uri   = NA
    outflows = NULL
  }
  
  if (cache) {
    tmp = list()
    tmp$base_catchments = network_list$catchments
    tmp$base_flowpaths = network_list$flowpaths
    
    write_hydrofabric(tmp,
                      cache_file,
                      verbose, 
                      enforce_dm = FALSE)
    
    rm(tmp)
  }
  
  if(!"member_comid" %in% names(network_list$flowpaths)){
    network_list$flowpaths$member_comid = NA
  }
  
  network_list = network_list |> 
    aggregate_along_mainstems(
    ideal_size_sqkm,
    min_area_sqkm,
    min_length_km,
    verbose = verbose,
    cache_file = cache_file
  ) |> collapse_headwaters2(
    min_area_sqkm,
    min_length_km,
    verbose = verbose,
    cache_file = cache_file)

  network_list$catchments = clean_geometry(network_list$catchments, ID = "id", keep = NULL)

if(!is.null(hydrolocations)){
  
  network_list$hydrolocations =  network_list$flowpaths %>% 
    st_drop_geometry() %>%
    select(id, poi_id) %>% 
    filter(!is.na(poi_id)) %>% 
    tidyr::separate_longer_delim(poi_id, delim = ",") %>% 
    left_join(mutate(select(hydrolocations, -id), poi_id = as.character(poi_id)), by = "poi_id",relationship = "many-to-many") %>% 
    distinct()

} 
  
  network_list$flowpaths = hydroloom::add_streamorder(network_list$flowpaths) 
  
  network_list$flowpaths = 
    select(network_list$flowpaths, 
           id, toid, 
           mainstem = levelpathid, 
           order = stream_order, 
           member_comid, poi_id, hydroseq, lengthkm, 
           areasqkm, tot_drainage_areasqkm = tot_drainage_area, has_divide) %>% 
    mutate(divide_id = ifelse(id %in% network_list$catchments$id, id, NA))
  
  topo = st_drop_geometry(network_list$flowpaths) %>% 
    select(divide_id, toid)
  
  network_list$divides = select(network_list$catchments, id,  areasqkm) %>% 
    mutate(divide_id = id, has_flowline = TRUE, ds_id = NA, type = "network") %>% 
    left_join(topo, by = "divide_id")
  
  network_list$catchments = NULL

  network_list$network  = st_drop_geometry(network_list$flowpaths) %>%
   select(
     id,
     toid,
     member  = member_comid,
     divide_id,
     any_of('poi_id'),
     mainstem,
     hydroseq,
     order,
     lengthkm, areasqkm,
     tot_drainage_areasqkm) %>%
   separate_longer_delim(col = 'member', delim = ",") %>%
   mutate(hf_id_part = sapply( strsplit(member, "[.]"), FUN = function(x){ x[2] }),
          hf_id_part = ifelse(is.na(hf_id_part), 1L, as.integer(hf_id_part)),
          hf_id = sapply( strsplit(member, "[.]"), FUN = function(x){ as.numeric(x[1]) }),
          member = NULL,
          hf_source = "NHDPlusV2"
   ) %>%
   left_join(st_drop_geometry(select(network_list$divides, divide_id, type, ds_id)), by = "divide_id")

  if(!is.null(vpu)){
    network_list$network$vpu = vpu
  } else {
    network_list$network$vpu = NA
  }

 
 if(!all(st_geometry_type(network_list$divides) == "POLYGON")){
   warning("MULTIPOLYGONS FOUND VPU: ", vpu)
 }
  

  if (!is.null(outfile)) {
  
    outfile = write_hydrofabric(
      network_list,
      outfile,
      verbose = verbose, 
      enforce_dm = FALSE)
    
    return(outfile)
    
  } else {
    network_list
  }
  
}
