#' Capture Network Metadata
#' This function assumes  that files are names *_{VPU}.gpkg
#' @param gpkgs a vector of file.paths to attribute
#' @param flowpath_layer the layer name containing flowpaths
#' @param divide_layer the layer name containing divides
#' @param network_layer the name of layer containing the hydrologic network
#' @return data.frame
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
  meta$cumcount_terminals = NA
  
  meta
  
}

#' Build a new ID table
#' @param gpkgs a row of network metadata built with `network_metadata`
#' @param flowpath_layer the layer name containing flowpaths
#' @param divide_layer the layer name containing divides
#' @param network_layer the name of layer containing the hydrologic network
#' @return data.frame
#' @export
#' @importFrom dplyr select filter mutate left_join inner_join
#' @importFrom sf read_sf

build_new_id_table = function(meta,
                              index, 
                              network_layer =  "network",
                              term_add = 1e9,
                              modifications = NULL){
  
  network = read_sf(meta$path[index], network_layer) %>% 
    select(id, toid, divide_id, hf_id)
  
  if(index == 1){
    term_add = 0 + term_add
  } else {
    term_add = meta$cumcount_terminals[index] + term_add
  }
 
  unqiue_ids = sort(unique(unlist(select(network, -hf_id))))
  
  terminals = filter(distinct(select(network, id, toid)), toid == 0 | is.na(toid)) %>% 
    mutate(terminal_id = term_add + 1:n()) %>% 
    select(oldID = id, terminal_id)
  
  df = data.frame(oldID = unqiue_ids) %>% 
    mutate(newID = c(1:length(unique(unqiue_ids)) + meta$cumcount_unique[index]),
           newID = ifelse(oldID == 0, 0, newID)) %>% 
    left_join(terminals, by = "oldID")
  
  if(!is.null(modifications)){

    lookup = network %>% 
      select(id, hf_id) %>%
      inner_join(modifications, by = "hf_id")
    
    df = left_join(df, lookup, by = c("oldID" = "id"))
  
  }
  
   df = distinct(df)
  
   df$terminals = nrow(terminals)
   
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

update_network_identifiers = function(x, lookup, term_add = 1e9, connections  = NULL){
  
  if("id" %in% names(x)){
    x$id = lookup$newID[match(x$id, lookup$oldID)]
  }
  
  if("ds_id" %in% names(x)){
    x$ds_id = lookup$newID[match(x$ds_id, lookup$oldID)]
  }
  
  if("divide_id" %in% names(x)){
    x$divide_id = lookup$newID[match(x$divide_id, lookup$oldID)]
  }
  
  if("toid" %in% names(x)){
    to   = lookup$newID[match(x$toid, lookup$oldID)]
    term = lookup$terminal_id[match(x$id, lookup$newID)]
    x$toid = ifelse(to != 0, to, term)
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
#' be globally unique.
#' @param gpkgs a vecotor of file.paths to that define the global network
#' @param outfiles a vector of file.paths to write to 
#' @param flowpath_layer the layer name containing flowpaths
#' @param divide_layer the layer name containing divides
#' @param network_layer the name of layer containing the hydrologic network
#' @param term_add value to be added to all terminal IDs
#' @param overwrite overwrite existing files?
#' @param verbose emit messages
#' @return a data.frame
#' @export 
#' @importFrom dplyr filter select mutate left_join arrange
#' @importFrom sf read_sf write_sf st_layers
#' @importFrom tidyr pivot_longer pivot_wider

assign_global_identifiers <- function(gpkgs                     = NULL, 
                                      outfiles                  = NULL,
                                      flowpath_layer            = "flowpaths",
                                      divide_layer              = "divides",
                                      network_layer             = "network",
                                      overwrite                 = FALSE,
                                      term_add                  = 1e9,
                                      modifications = NULL,
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
    select(from, to) %>% 
    mutate(connection = 1:n()) %>% 
    pivot_longer(-connection, names_to = "type", values_to = 'hf_id')
  
  ll = lapply(
    1:nrow(meta),
    FUN = function(l) {
      tmp = build_new_id_table(meta, l, modifications = modifications)
      tmp$unit = l
      meta$terminals[l] <<- tmp$terminals[1]
      meta$cumcount_terminals <<- c(0, head(cumsum(meta$terminals), -1))
      return(select(tmp, -terminals))
    }
  ) %>%  
    bind_rows()


  if(nrow(filter(ll, !is.na(terminal_id))) != sum(meta$terminals)){
    hyaggregate_log("FATAL", glue("Some terminal not found."), verbose)
  }
  
  if(nrow(filter(ll, !is.na(hf_id))) != nrow(modifications)){
      hyaggregate_log("FATAL", glue("Some modification connections not found."), verbose)
  }
  
  conn = filter(ll, !is.na(connection)) %>% 
    arrange(connection) %>% 
    select(newID, connection, type) %>% 
    pivot_wider(id_cols = connection, names_from = type, values_from = newID) %>% 
    select(connection, from, to)
  
  for(i in 1:nrow(meta)){
    lyrs   =  st_layers(meta$path[i])$name
    lookup =  filter(ll, unit == i)
    
    for(j in 1:length(lyrs)){
      
     hyaggregate_log("INFO", glue("Processing Unit {i}/{nrow(meta)} -- {lyrs[j]} ({j}/{length(lyrs)})"), verbose)
     
     update_network_identifiers(x = read_sf(gpkgs[i], lyrs[j]), lookup, term_add = term_add, connections = conn) %>% 
       write_sf(meta$outfiles[i], lyrs[j], overwrite = TRUE)
    }  
  }
}
