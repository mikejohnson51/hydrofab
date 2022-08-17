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

#' HyAggregate logging shorthand
#' Log a message with given log level, and optional verbosity.
#' @param level log level, see logger::log_levels for more details
#' @param message R objects that can be converted to a character vector via the active message formatter function
#' @param verbose should message be emitted?
#' @return log message
#' @export
#' @importFrom logger log_level
#' @importFrom glue glue

hyaggregate_log = function(level, message, verbose = TRUE){
  if(verbose){ log_level(level, message) }
}

#' Read Catchments and Flowpaths from Geopackage
#' Convenience function for reading two layers into a list
#' @param gpkg path to geopackage
#' @param catchment_name name of catchment layer. If NULL, attempts to find divides layer
#' @param flowpath_name name of flowpath layer. If NULL, attempts to find flowpath layer
#' @param crs desired CRS, if NULL they stay as read. If all CRS layers arenot
#' @param vebose should message be emitted?
#' @return list
#' @export

read_hydrofabric = function(gpkg = NULL,
                            catchments = NULL,
                            flowpaths = NULL,
                            crs = NULL,
                            verbose = TRUE){
  
  out = list()
  
  if(is.null(gpkg)){
    if(inherits(catchments, "sf")){ out[["catchments"]]  <- catchments }
    if(inherits(flowpaths, "sf")){ out[["flowpaths"]]  <- flowpaths }
  } else {
    
    hyaggregate_log("INFO", glue("\n--- Read in data from {gpkg} ---\n"), verbose)
    
    if(is.null(flowpaths)){
      flowpaths = grep("flowpath|flowline", st_layers(gpkg)$name, value = TRUE)
      if(length(flowpaths) > 1){ stop("Multiple flowpath names found.")}
      hyaggregate_log(level = "INFO",
                      message = glue("Reading flowpaths from: {flowpaths}"),
                      verbose)
    }
    
    if(is.null(catchments)){
      catchments = grep("divide|catchment", st_layers(gpkg)$name, value = TRUE)
      if(length(catchments) > 1){ stop("Multiple catchment names found.")}
      hyaggregate_log("INFO", glue("Reading catchments from: {catchments}"), verbose)
    }
    
    
    if(layer_exists(gpkg, flowpaths)){
      out[["flowpaths"]] <- read_sf(gpkg, flowpaths)
    }
    
    if(layer_exists(gpkg, catchments)){
      out[["catchments"]] <- read_sf(gpkg, catchments)
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


#' Download Reference Fabric Data by VPU ID
#'
#' @param VPU a VPU ID
#' @param type either 'refactor' (default) or 'reference'
#' @param dir directory path to save data to
#' @param overwrite should existing files be overwritten? (default = FALSE)
#' @return file path
#' @export
#' @importFrom nhdplusTools get_boundaries
#' @importFrom dplyr filter slice_min
#' @importFrom jsonlite fromJSON
#' @importFrom sbtools item_file_download
#' @importFrom httr GET write_disk

get_hydrofabric = function(VPU = "01",
                           type = "refactor",
                           dir  = NULL,
                           overwrite = FALSE) {
  if (is.null(dir)) {
    stop("`dir` cannot be NULL", call. = FALSE)
  }
  
  if(!VPU %in% get_boundaries()$VPUID){
    stop(VPU, " is not a valid VPU ID", call. = FALSE)
  }
  
  outfile = file.path(dir, paste0(type, "_", VPU, ".gpkg"))
  
  if (file.exists(outfile) & !overwrite) {
    return(outfile)
  } else {
    
    path = ifelse(
      type == "refactor",
      'https://www.sciencebase.gov/catalog/item/61fbfdced34e622189cb1b0a',
      #reference
      'https://www.sciencebase.gov/catalog/item/61295190d34e40dd9c06bcd7'
      # uniform
      # minimal
    )
    
    xx = fromJSON(paste0(path, '?format=json'), simplifyDataFrame = TRUE)
    
    find = slice_max(filter(xx$files, grepl(VPU, xx$files$name)), dateUploaded)
    
    tryCatch({
      sbtools::item_file_download(
        sb_id = basename(path),
        names = find$name,
        destinations = outfile,
        overwrite_file = TRUE
      )}, error = function(e){
        httr::GET(find$url, httr::write_disk(oufile, overwrite = TRUE))
      })
  }
  
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

write_hydrofabric = function(network_list,
                             outfile,
                             catchment_name = "divides",
                             flowpath_name  = "flowpaths",
                             edge_list_name = "edge_list",
                             verbose = TRUE){
  
  hyaggregate_log("SUCCESS", glue("Writing {flowpath_name} & {catchment_name} to {outfile}"), verbose)
  
  el = get_waterbody_edges_terms(network_list$flowpaths)
  
  names_nl = names(network_list)
  
  write_sf(network_list$flowpaths, outfile, flowpath_name)
  write_sf(network_list$catchments, outfile, catchment_name)
  write_sf(el, outfile, edge_list_name)
  
  if("mapped_POIs" %in% names_nl){
    write_sf(network_list[['mapped_POIs']], outfile, 'mapped_POIs')
  }
}


