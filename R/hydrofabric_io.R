#' Return ScienceBase ID for hydrofabric
#' This function checks if a layer exists in a geopackage
#' @param gpkg path to geopackage
#' @param name name of layer to check
#' @return character
#' @export

sb_id = function(type){
  
  if(type == "refactor") {
    id = '61fbfdced34e622189cb1b0a'
  } else if(type == "reference"){
    id = '61295190d34e40dd9c06bcd7'
  } else if(type == "uniform") {
    id = '629a4246d34ec53d276f446d'
  } else if(type == "minimal"){
    id = '60be1502d34e86b9389102cc'
  } else {
    stop(glue("{type} not valid"))
  }
  
  return(id)
}



#' Check if a geopackage and layer exists
#' This function checks if a layer exists in a geopackage
#' @param gpkg path to geopackage
#' @param name name of layer to check
#' @return logical
#' @export
#' @importFrom sf st_layers

layer_exists = function(gpkg, name){
  
  if(!file.exists(gpkg)){ return(FALSE) }
  
  if(name %in% st_layers(gpkg)$name){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#'Logging shorthand
#' Log a message with given log level, and optional verbosity.
#' @param level log level, see logger::log_levels for more details
#' @param message R objects that can be converted to a character vector via the active message formatter function
#' @param verbose should message be emitted?
#' @return log message
#' @export
#' @importFrom logger log_level
#' @importFrom glue glue

hyaggregate_log = function(level, message, verbose = TRUE){
  if(verbose){ 
    
    tryCatch({
      log_level(level, message)
    }, error = function(e){message(message)},
      warning = function(w){message(warning)}
    )
  }
}

#' Read Catchments and Flowpaths from Geopackage
#' Convenience function for reading two layers into a list
#' @param gpkg path to geopackage
#' @param catchment_name name of catchment layer. If NULL, attempts to find divides layer
#' @param flowpath_name name of flowpath layer. If NULL, attempts to find flowpath layer
#' @param realization what layers to read? Options: {"catchemnts", "flowpaths", "all"}
#' @param crs desired CRS, if NULL they stay as read. If all CRS layers arenot
#' @param vebose should message be emitted?
#' @return list
#' @export
#' @importFrom sf read_sf st_transform
#' @importFrom glue glue

read_hydrofabric = function(gpkg = NULL,
                            catchments = NULL,
                            flowpaths = NULL,
                            realization  =  "all",
                            crs = NULL,
                            verbose = Sys.getenv("hydrofab_verbose") != "false"){
  
  out = list()
  
  if(is.null(gpkg)){
    if(inherits(catchments, "sf")){ out[["catchments"]]  <- catchments }
    if(inherits(flowpaths, "sf")){  out[["flowpaths"]]   <- flowpaths }
  } else {
    
    gpkg = normalizePath(gpkg)
    hyaggregate_log(level = "INFO", message = glue("\n--- Read in data from {gpkg} ---\n"), verbose)
    
    if(is.null(flowpaths) & realization != "catchments"){
      flowpaths = grep("flowpath|flowline", st_layers(gpkg)$name, value = TRUE)
      flowpaths = flowpaths[!grepl("attributes|edge_list", flowpaths)]
      if(length(flowpaths) > 1){ stop("Multiple flowpath names found.")}
      hyaggregate_log(level = "INFO",
                      message = glue("Reading flowpaths from: {flowpaths}"),
                      verbose)
    }
    
    if(is.null(catchments) & realization != "flowpaths"){
      catchments = grep("divide|catchment", st_layers(gpkg)$name, value = TRUE)
      catchments = catchments[!grepl("network", catchments)]
      if(length(catchments) > 1){ stop("Multiple catchment names found.")}
      hyaggregate_log("INFO", glue("Reading catchments from: {catchments}"), verbose)
    }
    
    
    if(!is.null(flowpaths)){
      if(layer_exists(gpkg, flowpaths)){
        out[["flowpaths"]] <- read_sf(gpkg, flowpaths)
      }
    }
    
    if(!is.null(catchments)){
      if(layer_exists(gpkg, catchments)){
        out[["catchments"]] <- read_sf(gpkg, catchments)
      }
    }
  }
  
  if(!is.null(crs)){
    out = lapply(out, function(x){ st_transform(x, crs)})
  }
  
  crs_collection = lapply(out, st_crs)
  
  if(length(crs_collection) > 1){
    if(!identical(crs_collection[1], crs_collection[2])){
      out[[2]] = st_transform(out[[2]], st_crs(out[[1]]))
    }
  }
  
  return(out)
  
}


#' Write a hydrofabric gpkg
#' A hydrofabric consists of a flowpath, catchment,
#' and topology layer written to a self contained geopackage
#' @param network_list a list containing flowpaths and catchments
#' @param outfile a file (gpkg) where layers should be written
#' @param catchment_name the layer name for divides
#' @param flowpath_name the layer name for flowpaths
#' @param verbose should messages be emitted?
#' @return file path
#' @export

write_hydrofabric = function(network_list,
                             outfile,
                             verbose = TRUE, 
                             enforce_dm = TRUE){
  
  hyaggregate_log("SUCCESS", glue("Writing {length(network_list)} layers to {outfile}"), verbose)

  names_nl = names(network_list)
  
  if(!enforce_dm){
    if(length(names_nl) > 0){
    for(i in 1:length(names_nl)){
      write_sf(network_list[[names_nl[i]]], outfile, names_nl[i], overwrite = TRUE)
    }
  }
  
    return(outfile) 
  
  } else {

    if(!"WB" %in% names(network_list)){
     for(i in 1:length(hf_dm)){
       hf_dm[[names(hf_dm)[i]]] = select(hf_dm[[names(hf_dm)[i]]],
                                         -any_of("wb_id"))
     }
    }
    
    
   write_dm_model = function(data, dm, outfile, layer_name){
     
     if(is.null(data)){
       NULL
     } else {
       names = names(data)
       bad   = dm[!dm %in% names]
       
       if(length(bad) > 0){
         stop("Need extra parameters in ", layer_name, ": ", paste(bad, collapse = ", "), call.  = FALSE)
       } else {
         write_sf(data, outfile, layer_name)
       }
     }
     
   }
   
   write_dm_model(data = network_list$flowpaths, 
                  dm = names(hf_dm$flowlines), 
                  outfile, 
                  "flowpaths") 
   
   write_dm_model(data = network_list$divides,   
                  dm = names(hf_dm$divides), 
                  outfile, 
                  "divides") 
   
   write_dm_model(data = network_list$pois, 
                  dm = names(hf_dm$pois), 
                  outfile, 
                  "pois") 
   
   # write_dm_model(data = network_list$pois, 
   #                dm = names(hf_dm$hydrolocation_lookup), 
   #                outfile, 
   #                "hydrolocations_lookup") 
   
   write_dm_model(data = network_list$network, 
                  dm = names(hf_dm$network), 
                  outfile, "network") 
   
   # write_dm_model(data = network_list$network_lookup, 
   #                dm = names(hf_dm$network_lookup), 
   #                outfile, "network_lookup") 
   
   if("WB" %in% names(network_list)){
    write_dm_model(data = network_list$WB, dm = names(hf_dm$WB), outfile, "WB")
   }
   
   if("nexus" %in% names(network_list)){
     nex_dm = c('id', 'toid', 'hl_id', 'type')
     write_dm_model(data = network_list$nexus, dm = nex_dm, outfile, "nexus")
   }
   
    
   left_overs = names_nl[!names_nl %in% c(names(hf_dm), "nexus")]
   
   if(length(left_overs) > 0){
     lapply(1:length(left_overs), function(x){ write_sf(network_list[[left_overs[x]]], outfile, left_overs[x])})
   }
    
    return(outfile) 
    
  }
}

#' pack set
#' @param x data.frame containing "set" list column to be packed
#' @return data.frame containing comma seperated character column 
#' @export
pack_set <- function(x, y = "set") {
  x[[y]] <- sapply(x[[y]], paste, collapse = ",")
  x
}

#' unpack set
#' @param x data.frame containing comma separated "set" column to be unpacked
#' @return data.frame containing a list column
#' @export
unpack_set <- function(x, y = "set") {
  x[[y]] <- strsplit(x[[y]], ",")
  x
}
