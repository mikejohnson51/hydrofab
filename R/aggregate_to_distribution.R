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
  
  network_list = read_hydrofabric(gpkg,
                                  catchments = divide,
                                  flowpaths = flowpath,
                                  crs = 5070) 
  
  network_list$catchments <- clean_geometry(catchments = network_list$catchments, keep = NULL, ID = "ID")
  network_list            <- prepare_network(network_list)
  network_list            <- add_network_type(network_list, verbose = FALSE)
  
  # Add outlets
  if (!is.null(hydrolocations)) {
    
    outflows = hydrolocations %>% 
      st_as_sf() %>% 
      #st_drop_geometry() %>%
      select(id, starts_with("hl")) %>% 
      distinct() %>% 
      # Thu Nov  3 12:40:38 2022 ------------------------------
        # Why is this needed? 
        # https://code.usgs.gov/wma/nhgf/reference-hydrofabric/-/issues/114
      group_by(id) %>% 
      mutate(hl_reference = paste(hl_reference, collapse = ","),
             hl_id = paste(unique(hl_id), collapse = ","),
             hl_link = paste(unique(hl_link), collapse = ","),
             hl_position = paste(hl_position, collapse = ",")) %>% 
      slice(1) %>% 
      ungroup()

    network_list$flowpaths  = left_join(mutate(network_list$flowpaths, hl_id = NULL), 
                                        st_drop_geometry(outflows), 
                                        by = 'id')
  } else {
    network_list$flowpaths$hl_id   = NA
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
    cache_file = cache_file)


if(!is.null(hydrolocations)){
  tmp =  network_list$flowpaths %>% 
    st_drop_geometry() %>%
    select(id, hl_id) %>% 
    filter(!is.na(hl_id)) %>% 
    mutate(hl_id = hl_id) %>% 
    distinct() %>% 
    left_join(select(outflows, -id), 
              by = "hl_id",
              relationship = "many-to-many") %>% 
    st_as_sf() %>% 
    rename_geometry("geometry")
  
  hydrolocations_lookup =  select(st_drop_geometry(tmp), hl_id, id, hl_reference, hl_link, hl_position)
  
  hydrolocations = distinct(select(tmp, hl_id, id,  any_of(type), hl_position))
  
  network_list$hydrolocations = left_join(select(hydrolocations, -hl_position, -id), hydrolocations_lookup, by = "hl_id", relationship = "many-to-many") %>% 
    separate_longer_delim(cols =c('hl_reference', 'hl_link', 'hl_position'), delim = ",") %>% 
    mutate(hl_uri = paste0(hl_reference, "-", hl_link)) %>% 
    st_as_sf() %>% 
    select(hl_id, id, hl_reference, hl_link, hl_uri, hl_position)
}
  
 network_list$divides = select(network_list$catchments, id, toid, areasqkm) %>% 
   mutate(divide_id = id, has_flowline = TRUE, ds_id = NA, type = "network")
 
 network_list$flowpaths = 
   select(network_list$flowpaths, id, toid, mainstem = levelpathid, order, member_comid, any_of('hl_id'), hydroseq, lengthkm, 
          areasqkm, tot_drainage_areasqkm = tot_drainage_area, has_divide) %>% 
   mutate(divide_id = ifelse(id %in% network_list$divides$divide_id, id, NA))

 network_list$network  = st_drop_geometry(network_list$flowpaths) %>% 
   select(
     id,
     toid          = toid,
     member  = member_comid,
     divide_id,
     any_of('hl_id'),
     mainstem,
     hydroseq,
     order,
     lengthkm, areasqkm, tot_drainage_areasqkm) %>%
   separate_longer_delim(col = 'member', delim = ",") %>%
   mutate(hf_id_part = sapply( strsplit(member, "[.]"), FUN = function(x){ x[2] }),
          hf_id_part = ifelse(is.na(hf_id_part), 1L, as.integer(hf_id_part)),
          hf_id = sapply( strsplit(member, "[.]"), FUN = function(x){ as.numeric(x[1]) }),
          member = NULL,
          hf_source = "NHDPlusV2"
   ) %>% 
   left_join(st_drop_geometry(select(network_list$divides, divide_id, type, ds_id)), by = "divide_id")
 
 
 if(!is.null(hydrolocations)){
   network_list$network = network_list$network
   left_join(select(network_list$hydrolocations, hl_id, hl_uri), 
               by = "hl_id",
               relationship = "many-to-many") %>% 
     select(id, toid, divide_id, mainstem, hydroseq, 
            hf_source, hf_id, hf_id_part, 
            hl_id, hl_uri,
            hf_id, hf_source,
            lengthkm, areasqkm, tot_drainage_areasqkm
            ) 
 }
 
 
  if(!is.null(vpu)){ 
    network_list$network$vpu = vpu 
  } else {
    network_list$network$vpu = NA
  }
 
 network_list$catchments = NULL
 
  if (!is.null(outfile)) {
    outfile = write_hydrofabric(
      network_list,
      outfile,
      verbose = verbose, 
      enforce_dm = TRUE)
    
    return(outfile)
    
  } else {
    network_list
  }
  
}
