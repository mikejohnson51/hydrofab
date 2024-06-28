#' @title Prepare Hydrologic Network
#' @details This function adds an area, length, hydrosequence, streamorder and contributing drainage area
#' metric to the flowpath list element of network_list.
#' @details tot_drainage_areasqkm can only be added when there are no NA areas
#' @param network_list a list with flowpath and catchment data
#' @return a list containing flowpath and catchment `sf` objects
#' @export
#' @importFrom nhdplusTools get_streamorder calculate_total_drainage_area
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select

prepare_network = function(network_list) {
  
  names(network_list$flowpaths)  = tolower(names(network_list$flowpaths))
  names(network_list$catchments) = tolower(names(network_list$catchments))

  if(any(duplicated(network_list$catchments))){
    n = sum(duplicated(network_list$catchments))
    id = network_list$catchments$id[which(duplicated(network_list$catchments))]
    id = paste(id, collapse = ", ")
    hyaggregate_log("WARN", glue("Dropping {n} duplicate catchments: {id}"))
    network_list$catchments = filter(network_list$catchments, !duplicated(network_list$catchments))
  }
  
  if(any(duplicated(network_list$flowpaths))){
    n = sum(duplicated(network_list$flowpaths))
    id = network_list$flowpaths$id[which(duplicated(network_list$flowpaths))]
    id = paste(id, collapse = ", ")
    hyaggregate_log("WARN", glue("Dropping {n} duplicate flowpaths: {id}"))
    network_list$flowpaths = filter(network_list$flowpaths, !duplicated(network_list$flowpaths))
  }
  
  if(any(duplicated(network_list$flowpaths$id))){
    n = sum(duplicated(network_list$flowpaths$id))
    hyaggregate_log("WARN", glue("Dropping {n} duplicate flowpaths."))
  }

  # Add a hydrosequence to the flowpaths
  network_list$flowpaths = add_hydroseq(flowpaths = network_list$flowpaths)
  # Add area and length measures to the network list
  network_list = add_measures(network_list$flowpaths, network_list$catchments)
  
  network_list$flowpaths = mutate(network_list$flowpaths, areasqkm = ifelse(is.na(areasqkm), 0, areasqkm))
 #network_list$flowpaths$order = get_streamorder(st_drop_geometry(mutate(select(network_list$flowpaths, ID = id, toID = toid), divergence = 0)))
  network_list$flowpaths$tot_drainage_area = calculate_total_drainage_area(select(network_list$flowpaths, ID = id, toID = toid, area = areasqkm))
  
  check_network_validity(network_list)
}

#' Check Network Validity
#' Validates a flowpath and catchment network
#' @param term_cut cutoff integer to define terminal IDs
#' @return a list containing flowline and catchment `sf` objects
#' @noRd
#' @importFrom dplyr mutate select left_join
#' @importFrom sf st_drop_geometry

check_network_validity     <- function(network_list,
                                       term_cut = 1e9,
                                       check = TRUE){
  
  flowpaths = network_list$flowpaths
  cat       = network_list$catchments
  
  names(flowpaths) = tolower(names(flowpaths))
  names(cat) = tolower(names(cat))
  
  flowpaths$toid    = ifelse(is.na(flowpaths$toid), 0, flowpaths$toid)
  
  if(!check){ return(list(flowpaths = fl, catchments = cat))}
  
  DAG               = network_is_dag(flowpaths)
  CONNECTION        = sum(!(flowpaths$toid %in% flowpaths$id | flowpaths$toid > term_cut | flowpaths$toid == 0)) == 0
  
  if(all(DAG,  CONNECTION)){
    return(list(flowpaths = flowpaths, catchments = cat))
  } else {
    if(!DAG){ stop("Network is not a graph.")}
    if(!CONNECTION){stop("All toIDs are not present in network.")}
  }
}

#' Check if network is DAG
#' Checks in a `sf` flowline network is a DAG (Directed acyclic graph).
#' @param fl a LINESTRING `sf` flowlines object
#' @param ID the name of the ID column in `fl`
#' @param toID the name of the toID column in `fl`
#' @return boolean
#' @noRd
#' @importFrom igraph graph_from_data_frame is.dag
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select

network_is_dag = function(fl, ID = "id", toID = "toid"){
  st_drop_geometry(select(fl, !!ID, !!toID)) %>%
    graph_from_data_frame(directed = TRUE) %>%
    is.dag()
}

#' Add hydrosequence
#' @param flowpaths sf object (LINESTRING)
#' @return sf object
#' @export
#' @importFrom  nhdplusTools get_sorted 

add_hydroseq = function(flowpaths) {
  
  flowpaths$terminalID = NULL
  flowpaths$terminalid = NULL
  flowpaths$hydroseq   = NULL
  
  flowpaths$toid = ifelse(is.na(flowpaths$toid), 0, flowpaths$toid)
  
  topo = get_sorted(st_drop_geometry(select(flowpaths, id, toid)), split = FALSE)
  
  topo['hydroseq'] = 1:nrow(topo)
  
  left_join(flowpaths, select(topo, id, hydroseq), by = "id")
  
}

#' Add/sync/update length and area measures
#' @param cat sf object (POLYGON)
#' @return list
#' @export
#' @importFrom dplyr select left_join
#' @importFrom sf st_drop_geometry

add_measures = function(flowpaths, divides) {
  flowpaths$lengthkm  = add_lengthkm(flowpaths)
  divides$areasqkm = add_areasqkm(divides)
  flowpaths$areasqkm = NULL
  flowpaths = left_join(flowpaths,
                        select(st_drop_geometry(divides), id, areasqkm),
                        by = "id")
  list(flowpaths  = rename_geometry(flowpaths, "geometry"),
       catchments = rename_geometry(divides, "geometry"))
}

#' Compute length in kilometers
#' @param x LINESTRING sf object
#' @return numeric vector
#' @export
#' @importFrom units set_units drop_units
#' @importFrom sf st_length

add_lengthkm = function (x) { drop_units(units::set_units(st_length(x), "km")) }

#' Compute area in square kilometers
#' @param x POLYGON sf object
#' @return numeric vector
#' @export
#' @importFrom units set_units drop_units
#' @importFrom sf st_area

add_areasqkm = function (x) { drop_units(set_units(st_area(x), "km2")) }

#' Flush existing ID prefixes
#' Given a data object and column, remove a prefix and adjoining "-"
#' @param input input data object
#' @param col column to remove prefix from
#' @return data object with updated column
#' @export
flush_prefix = function(input, col) {
  for (i in col) {
    input[[i]] = as.numeric(gsub(".*-", "", input[[i]]))
  }
  input
}

#' Remove non-coincident Network Features
#' Remove non-coincident flowlines and catchment pairs from a network list
#' @param network_list a list containing flowpaths and catchments
#' @param verbose should message be emitted?
#' @return a list containing flowpaths and catchments
#' @export

drop_extra_features = function(network_list, verbose){
  
  network_list$flowpaths  =   network_list$flowpaths[!duplicated(network_list$flowpaths), ]
  network_list$catchments =   network_list$catchments[!duplicated(network_list$catchments), ]
  
  cond = describe_hydrofabric(network_list, verbose)
  
  if(!cond){
    
    bad_fps = filter(network_list$flowpaths, !id %in% network_list$catchments$id)$id
    
    if(length(bad_fps) > 0) {
      hyaggregate_log("WARN", glue("Removing flowpath(s): {paste(bad_fps, collapse = ', ')}"), verbose)
    }
    
    bad_cats = filter(network_list$catchments, !id %in% network_list$flowpaths$id)$id
    
    if(length(bad_cats) > 0) {
      hyaggregate_log("WARN", glue("Removing catchment(s): {paste(bad_cats, collapse = ', ')}"), verbose)
    }
    
    network_list = list(flowpaths = filter(network_list$flowpaths, id %in% network_list$catchments$id),
                        catchments = filter(network_list$catchments, id %in% network_list$flowpaths$id))
  }
  
  return(network_list)
  
}

#' Describe Hydrofabric
#' Describes a hydrofabric in terms of flowpath and catchment count. If they
#' are unequal, FALSE is returned. If equal TRUE is returned. Messages can optionally
#' be emitted.
#' @param network_list a list containing flowpaths and catchments
#' @param verbose should messages be emitted?
#' @return boolean condition
#' @export


describe_hydrofabric = function(network_list, verbose = TRUE){
  
  counts = sapply(network_list, nrow)
  
  if(counts[[1]] != counts[[2]]){
    
    hyaggregate_log("WARN",
                    glue("{counts[1]} {names(counts)[1]} vs. {counts[2]} {names(counts)[2]}"),
                    verbose)
    
    return(FALSE)
    
  } else {
    
    hyaggregate_log( "INFO",
                     glue("{counts[1]} features in network."),
                     verbose)
    
    return(FALSE)
    
  }
}
