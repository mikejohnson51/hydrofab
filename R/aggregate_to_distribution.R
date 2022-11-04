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
#' @importFrom dplyr left_join filter
#' @importFrom nhdplusTools get_sorted calculate_total_drainage_area get_streamorder
#' @importFrom logger log_appender appender_file appender_console

aggregate_to_distribution = function(gpkg = NULL,
                                     flowpath = NULL,
                                     divide = NULL,
                                     hydrolocations = NULL,
                                     ideal_size_sqkm = 10,
                                     min_length_km = 1,
                                     min_area_sqkm  = 3,
                                     outfile = NULL,
                                     log = TRUE,
                                     overwrite = FALSE,
                                     cache = TRUE,
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
  
  network_list = read_hydrofabric(gpkg,
                                  catchments = divide,
                                  flowpaths = flowpath,
                                  crs = 5070)
  
  network_list = add_network_type(network_list)

   # Add outlets
  if (!is.null(hydrolocations)) {
    
    outflows = select(hydrolocations, id, hl_id, hl_position) %>% 
      st_drop_geometry() %>% 
      filter(hl_position == "outflow") %>% 
      distinct() %>% 
      # Thu Nov  3 12:40:38 2022 ------------------------------
        # Why is this needed? 
        # https://code.usgs.gov/wma/nhgf/reference-hydrofabric/-/issues/114
      group_by(id) %>% 
      slice(1) %>% 
      ungroup()

    network_list$flowpaths  = left_join(network_list$flowpaths, 
                                        outflows, 
                                        by = 'id')
  } else {
    network_list$flowpaths$poi_id   = NA
  }
  

  network_list <- prepare_network(network_list)
  
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
  
  network_list = aggregate_along_mainstems(
    network_list,
    ideal_size_sqkm,
    min_area_sqkm,
    min_length_km,
    verbose = verbose,
    cache_file = cache_file
  )

  network_list  = collapse_headwaters2(
    network_list,
    min_area_sqkm,
    min_length_km,
    verbose = verbose,
    cache_file = cache_file
  )

 tmp =  network_list$flowpaths %>% 
    st_drop_geometry() %>%
    select(id, hl_id) %>% 
    filter(!is.na(hl_id)) %>% 
    mutate(hl_id = as.integer(hl_id)) %>% 
    left_join(select(hydrolocations, -id), 
              by = "hl_id") %>% 
   st_as_sf() %>% 
   rename_geometry("geometry")
 
 network_list$hydrolocations_lookup = 
   st_drop_geometry(tmp) %>% 
   select(hl_id, id, hl_reference, hl_link, hl_position)
 
 network_list$hydrolocations = 
   select(tmp, hl_id, id, hl_position) %>% 
   distinct()
 
  if (!is.null(outfile)) {
    outfile = write_hydrofabric(
      network_list,
      outfile,
      verbose = verbose, 
      enforce_dm = FALSE)
    
    return(outfile)
    
  } else {
    tmp
  }
  
}