#' Capture Network Metadata
#' This function assumes  that files are names *_{VPU}.gpkg
#' @param gpkgs a vector of file.paths to attribute
#' @param flowpath_layer the layer name containing flowpaths
#' @param divide_layer the layer name containing divides
#' @return a data.frame with the file.path, file name, VPU,flowpath count, 
#' divides count, cumulative flowpath count and cummulative divdies count
#' @export
#' @importFrom dplyr mutate
#' @importFrom sf st_layers

network_metadata = function(gpkgs,
                            flowpath_layer = "flowpaths",
                            divide_layer   = "divides"){
  
  meta = data.frame(path = gpkgs) %>% 
    mutate(VPU =  sub('.*_', '', gsub(".gpkg", "", basename(path)))) 

  for(i in 1:nrow(meta)){
    t = st_layers(meta$path[i])
    meta$flowpaths[i] =  t$features[which(t$name == flowpath_layer)]
    meta$divides[i]   =  t$features[which(t$name == divide_layer)]
  }
  
  meta$cumcount_fl  = c(0, head(cumsum(meta$flowpaths), -1))
  meta$cumcount_div = c(0, head(cumsum(meta$divides), -1))
  meta$terminals = NA
  
  meta
  
}

#' Build a new ID lookup table
#' @param gpkgs a row of network metadata built with `network_metadata`
#' @param flowpath_layer the layer name containing flowpaths
#' @param divide_layer the layer name containing divides
#' @return a data.frame with the file.path, file name, VPU,flowpath count, 
#' divides count, cumulative flowpath count and cumulative divides count
#' @export
#' @importFrom dplyr select mutate left_join
#' @importFrom sf read_sf st_drop_geometry

build_new_id_table = function(x, 
                              flowpath_layer = "flowpaths",
                              divide_layer   = "divides"){
  
  div = read_sf(x$path, divide_layer) %>%
    renamer()
  
  fl = read_sf(x$path, flowpath_layer) %>% 
    st_drop_geometry() %>%
    renamer()
  
  if("set" %in% names(fl)) {
    fl <- select(fl, oldID = id, set)
  } else {
    fl = select(fl, oldID = id, member_comid)
  }
    
  data.frame(oldID = sort(div$id)) %>% 
    mutate(newID = 1:length(unique(div$id)) + x$cumcount_div,
           newID = ifelse(oldID == 0, 0, newID),
           VPU = x$VPU) %>% 
    left_join(fl, by = "oldID")
}

#' Update Network Identifiers
#' Given a data.frame of sf object, the id and toid values are undated based on
#' a provided lookup table (produced with build_new_id_table), and a vpu_topo list 
#' if there are cross VPU flows. In the vpu_topo is NULL or has 0 rows, no vpu correction
#' is applied.
#' @param x a data.frame or sf object with id and/or toid columns
#' @param lookup a lookup table of new ID values
#' @param vpu_topo a VPU lookup correction table
#' @return data.frame
#' @export
#' @importFrom dplyr filter bind_rows

update_topo = function(x, lookup, vpu_topo = NULL){
  
  if("id" %in% names(x)){
    x$id = lookup$newID[match(x$id, lookup$oldID)]
  }
  
  if("toid" %in% names(x)){
    x$toid = lookup$newID[match(x$toid, lookup$oldID)]
    x$toid[is.na(x$toid)] = 0
  }
  
  if((nrow(vpu_topo) != 0 | is.null(vpu_topo)) & "toid" %in% names(x)){

    good = filter(x, !id %in% vpu_topo$ID)
    fix  = filter(x,   id %in% vpu_topo$ID)
    
    fix$toid = vpu_topo$toID[match(fix$id, vpu_topo$ID)]
    
    x = bind_rows(good, fix)
  }
  
  if("id" %in% names(x)){
    x$id = as.integer(x$id)
  }
  
  if("toid" %in% names(x)){
    x$toid = as.integer(x$toid)
  }
  
  x
}

#' Update Hydrofabric Identifiers
#' For a given set of hydrofabric geopackages, update the ID and toID values to 
#' be gloablaly unique.
#' @param gpkgs a vecotor of file.paths to that define the global network
#' @param outfiles a vector of file.paths to write to 
#' @param flowpath_layer the layer name containing flowpaths
#' @param mapped_POI_layer the layer name containing flowpaths
#' @param divide_layer the layer name containing divides
#' @param lookup_table_layer the layer name containing flowpaths
#' @param catchment_network_layer the layer name containing flowpaths
#' @param overwrite overwrite existing files?
#' @param verbose emit messages
#' @return a data.frame
#' @export 
#' @importFrom dplyr filter select mutate left_join
#' @importFrom sf read_sf write_sf
#' @importFrom tidyr unnest

assign_global_identifiers <- function(gpkgs                     = NULL, 
                                      outfiles                  = NULL,
                                      flowpath_layer            = "flowpaths",
                                      mapped_POI_layer          = "mapped_POIs",
                                      divide_layer              = "divides",
                                      lookup_table_layer        = "lookup_table",
                                      overwrite                 = FALSE,
                                      update_terminals          = TRUE,
                                      term_add                  = 1e9,
                                      return_lookup             = TRUE,
                                      verbose                   = TRUE) {


  meta = network_metadata(gpkgs, flowpath_layer, divide_layer)

  lus = list()
  
  if(is.null(outfiles) & !overwrite){
    stop("No outfiles given and overwrite = FALSE")
  }
  
  if(!is.null(outfiles) & length(outfiles) != length(gpkgs)){
    stop("length(outfiles) is less then length(gpkgs)")
  }
  
  if(is.null(outfiles) & overwrite){
    meta$outfiles = gpkgs
  } else {
    meta$outfiles = outfiles
  }
  
  gs_file = 'https://code.usgs.gov/wma/nhgf/reference-hydrofabric/-/raw/04cd22f6b5f3f53d10c0b83b85a21d2387dfb6aa/workspace/cache/rpu_vpu_out.csv'
  
  vpu_topo_all = read.csv(gs_file) %>% 
    filter(VPUID != toVPUID) %>% 
    select(VPUID, toVPUID, COMID, toCOMID)
  
  for(i in 1:nrow(meta)) {
    
   hyaggregate_log("INFO", glue("Processing VPU-{meta$VPU[i]}..."), verbose)
   
   lu = build_new_id_table(x = meta[i,], flowpath_layer, divide_layer)
  
   vpu_topo = filter(vpu_topo_all, VPUID == meta$VPU[i]) 
   
   if(nrow(vpu_topo) > 0){
     hyaggregate_log("INFO", glue("Building Downstream VPU list for VPU-{vpu_topo$toVPUID[1]}..."), verbose)
     
     ds_vpu  =  filter(meta, VPU %in% vpu_topo$toVPUID) %>% 
       build_new_id_table(flowpath_layer, divide_layer)
     
     vpu_topo = filter(lu, grepl(paste(vpu_topo$COMID, collapse = "|"),
                                 lu$member_comid)) %>% 
       mutate(member_comid = strsplit(member_comid, split = ",")) %>% 
       unnest('member_comid') %>% 
       filter(member_comid %in% vpu_topo$COMID) %>% 
       mutate(member_comid = as.integer(member_comid)) %>% 
       select(ID = newID, COMID = member_comid) %>% 
       left_join(vpu_topo, by = "COMID")
     
     vpu_topo = filter(ds_vpu, grepl(paste(vpu_topo$toCOMID, collapse = "|"), 
                                     ds_vpu$member_comid)) %>% 
       mutate(member_comid = strsplit(member_comid, split = ",")) %>% 
       unnest('member_comid') %>% 
       filter(member_comid %in% vpu_topo$toCOMID) %>% 
       mutate(member_comid = as.integer(member_comid)) %>% 
       select(toID = newID, toCOMID = member_comid) %>% 
       left_join(vpu_topo, by = "toCOMID")
    }
    
   lu$member_comid = NULL
   
   ## Flowpaths ##
     if(layer_exists(meta$path[i], flowpath_layer)){
      
       fl = read_sf(meta$path[i], flowpath_layer) %>% 
         renamer() %>%
         update_topo(lu, vpu_topo)
      
       meta$terminals[i] = sum(fl$toid == 0 | is.na(fl$toid))
       
       write_sf(fl, meta$outfiles[i], flowpath_layer, overwrite = TRUE)
      
    } else {
      stop(flowpath_layer, " does not exist!")
    }
   
    
    ## Divides ##
    if(layer_exists(meta$path[i], divide_layer)){

      dv = read_sf(meta$path[i], divide_layer) %>% 
        update_topo(lu, vpu_topo)  %>% 
        renamer() %>%
        update_topo(lu, vpu_topo)
      
      write_sf(dv, meta$outfiles[i], divide_layer, overwrite = TRUE)
      
    } else {
      stop(divide_layer, " does not exist!")
    }
    
    ### mapped_POIs ###
    if(layer_exists(meta$path[i], mapped_POI_layer)){
      read_sf(meta$path[i], mapped_POI_layer) %>% 
        renamer() %>%
        update_topo(lu, vpu_topo) %>% 
        write_sf(meta$outfiles[i], mapped_POI_layer, overwrite = TRUE)
    } else {
      stop(mapped_POI_layer, " does not exist!")
    }
    
    ### lookup_table ###
    if(layer_exists(meta$path[i], lookup_table_layer)){
      read_sf(meta$path[i], lookup_table_layer) %>% 
        renamer() %>% 
        update_topo(lu, vpu_topo)  %>% 
        rerenamer(lookup = TRUE) %>% 
        write_sf(meta$outfiles[i], lookup_table_layer, overwrite = TRUE)

    } else {
      stop(lookup_table_layer, " does not exist!")
    }

    # ### catchment_network ###
    #  if(layer_exists(meta$path[i], catchment_network_layer)){
    #   read_sf(meta$path[i], catchment_network_layer) %>% 
    #     renamer() %>%
    #     update_topo(lu, vpu_topo)  %>% 
    #     write_sf(meta$outfiles[i], catchment_network_layer, overwrite = TRUE)
    #   } else {
    #       stop(catchment_network_layer, " does not exist!")
    #   }

    hyaggregate_log("INFO", glue("Finished VPU-{meta$VPU[i]}!"), verbose)
    lus[[i]] = lu
  }
  
  meta$cumcount_term = c(0, head(cumsum(meta$terminals), -1))
  
  lookup = bind_rows(lus) %>% 
    select(VPU, oldID, newID)

  
  if(update_terminals){
    meta = assign_global_terminal_identifiers(meta, 
                                              flowpath_layer = flowpath_layer, 
                                              divide_layer = divide_layer,
                                              lookup_table_layer   = lookup_table_layer,
                                              #catchment_network_layer   = catchment_network_layer,
                                              term_add = term_add,
                                              verbose = verbose
    )
  }

  
  if(return_lookup){
    list(meta = meta, lookup = lookup)
  } else {
    return(meta)
  }
}



#' Update Hydrofabric Terminal Identifiers
#' For a given set of hydrofabric geopackages, update terminal toID values to 
#' be gloablaly unique.
#' @param gpkgs a metadata data.frame resulting from `assign_global_identifiers`
#' @param flowpath_layer the layer name containing flowpaths
#' @param lookup_table_layer the layer name containing flowpaths
#' @param catchment_network_layer the layer name containing flowpaths
#' @param term_add a numeric value to add to the terminal ids to facilitate 
#' easy discovery, and to avoid conflict with know IDs
#' @param verbose emit messages
#' @return data.frame
#' @export
#' @importFrom dplyr select mutate filter bind_rows left_join
#' @importFrom sf read_sf write_sf st_drop_geometry
#' @importFrom glue glue

assign_global_terminal_identifiers = function(meta, 
                                              flowpath_layer = "flowpaths",
                                              divide_layer   = "divides",
                                              lookup_table_layer   = "lookup_table",
                                              catchment_network_layer   = "catchment_network",
                                              verbose = TRUE,
                                              term_add = 1e9){
  
  for(i in 1:nrow(meta)){
    
    hyaggregate_log("INFO", glue("Processing VPU-{meta$VPU[i]} terminals..."), verbose)
    
    ## Flowpaths ##
    if(layer_exists(meta$outfiles[i], flowpath_layer)){
      
       fl = read_sf(meta$outfiles[i], flowpath_layer) 
       
       topo = select(fl, id, toid) %>% 
         st_drop_geometry()
       
       terms = filter(topo, toid == 0)
       
       if(nrow(terms) > 0){
         terms = terms %>% 
           mutate(tmp_id = 1:n()) %>% 
           mutate(toid = tmp_id + term_add + meta$cumcount_term[i],
                  toid = as.integer(toid),
                  tmp_id = NULL)
       }
    
       
       topo = filter(topo, !id %in% terms$id) %>% 
         bind_rows(terms)
    
      fl = mutate(fl, toid = NULL) %>% 
         left_join(topo, by = 'id') %>% 
         select(id, toid, everything()) 
      
      write_sf(fl, meta$outfiles[i], flowpath_layer, overwrite = TRUE)
       
     } else {
       stop(flowpath_layer, " does not exist!")
     }
    
    
    ## Update Catchments ##
    if(layer_exists(meta$outfiles[i], divide_layer)){
      
      cat = read_sf(meta$outfiles[i], divide_layer) %>% 
        mutate(toid = NULL) %>% 
        left_join(st_drop_geometry(select(fl, id, toid)), by = 'id') %>% 
        select(id, toid, everything()) 

      write_sf(cat, meta$outfiles[i], divide_layer, overwrite = TRUE)
  
    } else {
      stop(divide_layer, " does not exist!")
    }
    
 
     ### lookup_table ###
     if(layer_exists(meta$outfiles[i], lookup_table_layer)){
       
       lookup = read_sf(meta$outfiles[i], lookup_table_layer) %>% 
         mutate(toID = NULL) %>% 
         left_join(topo, by = c('aggregated_flowpath_ID' = 'id')) %>% 
         rename(toid = toID) %>% 
         select(NHDPlusV2_COMID, NHDPlusV2_COMID_part,
                reconciled_ID, aggregated_flowpath_ID,      
                toID, mainstem, POI_ID, POI_TYPE, POI_VALUE)
       
        write_sf(lookup, meta$outfiles[i], lookup_table_layer, overwrite = TRUE)
        
     } else {
       stop(lookup_table_layer, " does not exist!")
     }
     
     ### catchment_network ###
     # if(layer_exists(meta$outfiles[i], catchment_network_layer)){
     #    cn = select(st_drop_geometry(fl), id, toid, lengthkm, areasqkm, levelpathid)
     #    write_sf(cn, meta$outfiles[i], catchment_network_layer, overwrite = TRUE)
     # } else {
     #   stop(catchment_network_layer, " does not exist!")
     # }
     
     hyaggregate_log("INFO", glue("Finished VPU-{meta$VPU[i]}!"), verbose)

  }
  
 meta
}

#' @importFrom dplyr rename any_of
renamer <- function(x) {
  rename(x, any_of(c(id = "aggregated_ID", 
                     id = "ID", 
                     toid = "toID",
                     member_comid = "member_COMID",
                     id = "aggregated_flowpath_ID",
                     did = "aggregated_divide_ID",
                     levelpathid = "LevelPathID")))
}

rerenamer <- function(x, agg = FALSE, lookup = FALSE) {
  if(agg) {
    check <- c(aggregated_ID = "id",
               toID = "toid",
               member_COMID = "member_comid")
  } else if(lookup) {
    check <- c(aggregated_flowpath_ID = "id",
               aggregated_divide_ID = "did")
  } else {
    check <- c(ID = "id", 
               toID = "toid")
  }
  rename(x, any_of(check))
}

# TODO: replace read_sf calls with these.
hy_read <- function(x, layer, scheme = NULL) {
  # if inherits, data.frame, return
  # try to read gpkg layer
  # rename if scheme is specified
}

hy_write <- function(x, gpkg, layer, scheme = NULL) {
  # rename if scheme is specified
  # write out
}