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
  
  network_list = read_hydrofabric(gpkg,
                                  catchments = divide,
                                  flowpaths = flowpath,
                                  crs = 5070) 
  

  #network_list$catchments <- clean_geometry(catchments = network_list$catchments, keep = NULL, ID = "ID")
  network_list            <- prepare_network(network_list)
  network_list            <- add_network_type(network_list, verbose = FALSE)
  
  # Add outlets
  if (!is.null(hydrolocations)) {
    
    names(hydrolocations) = tolower(names(hydrolocations))
    
    outflows = hydrolocations %>% 
      select(hf_id, id, vpuid,  starts_with("hl")) %>% 
      group_by(id) %>% 
      mutate(hl_reference = paste(hl_reference, collapse = ","),
             hl_id = paste(hl_id, collapse = ","),
             hl_link = paste(hl_link, collapse = ","),
             hl_position = paste(hl_position, collapse = ",")) %>% 
      slice(1) %>% 
      ungroup() %>% 
      mutate(hl_id = paste0(vpuid, 1:n()))

    network_list$flowpaths  = left_join(mutate(network_list$flowpaths, hl_id = NULL), 
                                        st_drop_geometry(outflows), 
                                        by = 'id')
  } else {
    network_list$flowpaths$hl_id   = NA
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
  
  network_list2 = aggregate_along_mainstems(
    network_list,
    ideal_size_sqkm,
    min_area_sqkm,
    min_length_km,
    verbose = verbose,
    cache_file = cache_file
  )

  network_list3  = collapse_headwaters2(
    network_list2,
    min_area_sqkm,
    min_length_km,
    verbose = verbose,
    cache_file = cache_file)

  network_list3$catchments = clean_geometry(network_list3$catchments, ID = "id")
  
  #write_sf(network_list3$catchments, cache_file, "cleaned_step")

if(!is.null(hydrolocations)){
  
  hydrolocations =  network_list3$flowpaths %>% 
    st_drop_geometry() %>%
    select(id, hl_id) %>% 
    filter(!is.na(hl_id)) %>% 
    distinct() %>% 
    left_join(select(outflows, -id), 
              by = "hl_id",
              relationship = "many-to-many") %>% 
    st_as_sf() %>% 
    rename_geometry("geometry") %>% 
    distinct()
  
  hydrolocations_lookup =  select(st_drop_geometry(hydrolocations), 
                                  starts_with("hl_"),
                                  id)

  network_list3$hydrolocations = hydrolocations %>% 
    separate_longer_delim(cols =c('hl_reference', 'hl_link', 'hl_position'), delim = ",") %>% 
    mutate(hl_uri = paste0(hl_reference, "-", hl_link)) %>% 
    st_as_sf() %>% 
    select(hl_id, id, hl_reference, hl_link, hl_uri, hl_position)
} 
  
  network_list3$flowpaths = 
    select(network_list3$flowpaths, id, toid, mainstem = levelpathid, order, member_comid, any_of('hl_id'), hydroseq, lengthkm, 
           areasqkm, tot_drainage_areasqkm = tot_drainage_area, has_divide) %>% 
    mutate(divide_id = ifelse(id %in% network_list3$catchments$id, id, NA))
  
  topo = st_drop_geometry(network_list3$flowpaths) %>% 
    select(divide_id, toid)
  
  network_list3$divides = select(network_list3$catchments, id,  areasqkm) %>% 
    mutate(divide_id = id, has_flowline = TRUE, ds_id = NA, type = "network") %>% 
    left_join(topo, by = "divide_id")
  
  network_list3$catchments = NULL

  network_list3$network  = st_drop_geometry(network_list3$flowpaths) %>% 
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
   left_join(st_drop_geometry(select(network_list3$divides, divide_id, type, ds_id)), by = "divide_id")
 
 
 if(is.null(network_list3$network$hl_uri)){
   network_list3$network$hl_uri = NA
 }
 
  if(!is.null(vpu)){ 
    network_list3$network$vpu = vpu 
  } else {
    network_list3$network$vpu = NA
  }

 
 if(!all(st_geometry_type(network_list3$divides) == "POLYGON")){
   stop("MULTIPOLYGONS FOUND VPU: ", vpu)
 }
  
  if(!all(st_geometry_type(network_list3$flowpaths) == "LINESTRING")){
    
    # line_merge = function(x){
    #   
    #   ls = x[st_geometry_type(x) == "LINESTRING", ]
    #   ms = x[!st_geometry_type(x) == "LINESTRING", ]
    #   
    #   d = filter(network_list3$divides, id %in% ms$id)
    #   mapview::mapview(ms) + d
    #   
    # }
    # 
    # tmp2 = st_line_merge(network_list3$flowpaths)
    # 
    # if(nrow(tmp2) == nrow(network_list3$flowpaths)){
    #   network_list3$flowpaths = tmp2
    # } else {
      warning("MULTILINESTRINGS FOUND VPU: ", vpu)
    #}
  }
 
  if (!is.null(outfile)) {
  
    outfile = write_hydrofabric(
      network_list3,
      outfile,
      verbose = verbose, 
      enforce_dm = TRUE)
    
    return(outfile)
    
  } else {
    network_list3
  }
  
}
