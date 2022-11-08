summarize_hf = function(gpkg){
  
  tmp = read_hydrofabric(gpkgs[1])
  
  x = read_sf(gpkgs[3], "network")
  
  table(x$network_type)
  
  st_layers(gpkgs[3])
}


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
                            divide_layer   = "divides",
                            network_layer  = "network"){
  
  meta = data.frame(path = gpkgs) %>% 
    mutate(VPU =  sub('.*_', '', gsub(".gpkg", "", basename(path)))) 

  for(i in 1:nrow(meta)){
    t = st_layers(meta$path[i])
    meta$flowpaths[i] =  t$features[which(t$name == flowpath_layer)]
    meta$divides[i]   =  t$features[which(t$name == divide_layer)]
    meta$unique_features[i]   =  t$features[which(t$name == network_layer)]
  }
  
  meta$cumcount_fl  = c(0, head(cumsum(meta$flowpaths), -1))
  meta$cumcount_div = c(0, head(cumsum(meta$divides), -1))
  meta$cumcount_unique = c(0, head(cumsum(meta$unique_features), -1))
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
                              network =  "network", 
                              network_lookup = "network_lookup",
                              modifications = NULL){
  
  network = read_sf(x$path, network) %>% 
    select(id, toid, divide_id, toid)
  
  unqiue_ids = sort(unique(unlist(network)))
  
  df = data.frame(oldID = unqiue_ids) %>% 
    mutate(newID = c(1:length(unique(unqiue_ids)) + x$cumcount_unique),
           newID = ifelse(oldID == 0, 0, newID))
  
  if(!is.null(modifications)){

    lookup = read_sf(x$path, network_lookup) %>% 
      filter(hf_id_part == 1) %>% 
      select(id, hf_id) %>%
      inner_join(modifications, by = "hf_id")
    
    df = left_join(df, lookup, by = c("oldID" = "id"))
  
  }
  
   df$terminals = sum(network$toid == 0 | is.na(network$toid))
   return(df)

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

update_topo = function(x, lookup, connections  = NULL){
  
  if("id" %in% names(x)){
    x$id = lookup$newID[match(x$id, lookup$oldID)]
  }
  
  if("divide_id" %in% names(x)){
    x$divide_id = lookup$newID[match(x$divide_id, lookup$oldID)]
  }
  
  if("toid" %in% names(x)){
    x$toid = lookup$newID[match(x$toid, lookup$oldID)]
    x$toid[is.na(x$toid)] = 0
  }
  
  if(!is.null(connections) & "toid" %in% names(x)){
    if("id" %in% names(x)){
    x = filter(x, id %in% connections$from) %>% 
      select(-toid) %>% 
      left_join(select(connections, id = from, toid = to), by = "id") %>% 
      bind_rows(filter(x, !id %in% connections$from))
    } else {
      x = filter(x, divide_id %in% connections$from) %>% 
        select(-toid) %>% 
        left_join(select(connections, divide_id = from, toid = to), by = "divide_id") %>% 
        bind_rows(filter(x, !divide_id %in% connections$from))
    }
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
                                      mapped_POI_layer          = "POIs",
                                      divide_layer              = "divides",
                                      lookup_table_layer        = "network_lookup",
                                      network_layer             = "network",
                                      overwrite                 = FALSE,
                                      update_terminals          = TRUE,
                                      term_add                  = 1e9,
                                      return_lookup             = TRUE,
                                      verbose                   = TRUE) {


  meta = network_metadata(gpkgs, flowpath_layer, divide_layer, network_layer)

  
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
  
  
  modifications = modifications %>% 
    mutate(connection = 1:n()) %>% 
    pivot_longer(-connection, names_to = "type", values_to = 'hf_id')
  
  ll = lapply(
    1:nrow(meta),
    FUN = function(l) {
      tmp = build_new_id_table(x = meta[l,], modifications = modifications)
      tmp$unit = l
      meta$terminals[l] <<- tmp$terminals[1]
      return(select(tmp, -terminals))
    }
  ) %>%  
    bind_rows()
  
  meta$cumcount_term = c(0, head(cumsum(meta$terminals), -1))
     
  if(nrow(filter(ll, !is.na(hf_id))) != nrow(modifications)){
      hyaggregate_log("FATAL", glue("Some modification connections not found."), verbose)
  }
  
  conn = filter(ll, !is.na(connection)) %>% 
    arrange(connection) %>% 
    select(newID, connection, type) %>% 
    tidyr::pivot_wider(connection, names_from = type, values_from = newID) %>% 
    select(connection, from, to)
  
  for(i in 1:nrow(meta)){
    lyrs = st_layers(meta$path[i])$name
    lookup =  filter(ll, unit == i)
    
    for(j in 1:length(lyrs)){
     tmp = read_sf(gpkgs[i], lyrs[j])
     hyaggregate_log("INFO", glue("Processing Unit {i}/{nrow(meta)} -- {lyrs[j]}"), verbose)
     update_topo(x = tmp, lookup, connections = conn) %>% 
      write_sf(meta$outfiles[i], lyrs[j], overwrite = TRUE)
    }  
  }
  
  

  if(update_terminals){
    meta = assign_global_terminal_identifiers(meta, 
                                              flowpath_layer = flowpath_layer, 
                                              divide_layer = divide_layer,
                                              lookup_table_layer   = lookup_table_layer,
                                              flowpath_edge_list   = flowpath_edge_list,
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
                                              flowpath_edge_list   = "flowpath_edge_list",
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
         renamer() %>%
         mutate(toid = NULL) %>% 
         left_join(topo, by = "id") %>% 
         rename(toid = toID)
       
       if("POI_ID" %in% names(lookup)) {
         lookup <- select(lookup, NHDPlusV2_COMID, NHDPlusV2_COMID_part,
                          reconciled_ID, aggregated_flowpath_ID = id,      
                          toID, mainstem, POI_ID, POI_TYPE, POI_VALUE)
       } else {
         lookup <- select(lookup, NHDPlusV2_COMID, 
                          reconciled_ID = id, 
                          member_COMID = member_comid)
       }
       
        write_sf(lookup, meta$outfiles[i], lookup_table_layer, overwrite = TRUE)
        
     } else {
       stop(lookup_table_layer, " does not exist!")
     }

     ## catchment_network ###
     if(layer_exists(meta$outfiles[i], flowpath_edge_list)){
        cn = select(st_drop_geometry(fl), id, toid)
        write_sf(cn, meta$outfiles[i], flowpath_edge_list, overwrite = TRUE)
     } else {
       stop(flowpath_edge_list, " does not exist!")
     }

     hyaggregate_log("INFO", glue("Finished VPU-{meta$VPU[i]}!"), verbose)

  }
  
 meta
}

#' @importFrom dplyr rename any_of
renamer <- function(x) {
  
  rules <- c(id = "aggregated_ID", 
             id = "ID", 
             toid = "toID",
             member_comid = "member_COMID",
             id = "aggregated_flowpath_ID",
             did = "aggregated_divide_ID",
             levelpathid = "LevelPathID")
  
  if(sum(c("reconciled_ID", "aggregated_ID", "aggregated_flowpath_ID") %in% names(x)) < 2) {
    rules <- c(rules, id = "reconciled_ID")
  }
  
  rename(x, any_of(rules))
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