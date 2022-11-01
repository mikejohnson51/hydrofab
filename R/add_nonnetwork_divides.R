#' Add Non Network Divides to Aggregated Fabric
#' The refactoring process intentionally drop catchments without a flowpath.
#' In cases where a seamless discritization of the landscape is needed, these 
#' area must be reintroduced from the reference dataset.
#' @param gpkg a path to a gpkg
#' @param divide If gpkg is NULL, then an sf data.frame, otherwise a the layer name. See details.
#' @param flowpath If gpkg is NULL, then an sf data.frame, otherwise a the layer name. See details.
#' @param reference_gpkg A path to the reference VPU geopackage (see get_hydrofabric(..., type = "reference"))
#' @param verbose Should messages be emitted?
#' @return gpkg path
#' @export
#' @importFrom sf st_layers st_transform st_crs write_sf
#' @importFrom dplyr filter select rename mutate bind_rows
#' @importFrom nhdplusTools rename_geometry


add_nonnetwork_divides = function(gpkg = NULL,
                                  divides = NULL,
                                  reference_gpkg = NULL,
                                  verbose = TRUE){
  
  if(is.null(reference_gpkg)){ stop('reference_gpkg cannot be NULL')}
  
  ref_nl = read_hydrofabric(reference_gpkg, verbose = verbose, realization = "catchments")
  
  catchment_name = grep("divide|catchment", st_layers(gpkg)$name, value = TRUE)
  catchment_name = catchment_name[!grepl("network", catchment_name)]
  
  out_nl = read_hydrofabric(gpkg, 
                            catchments = catchment_name, 
                            flowpaths = NULL,
                            verbose = verbose,
                            realization = "all")
  
  # Encapsulated Flows
  u_fl = unique(as.integer(unlist(strsplit(out_nl$flowpaths$member_comid, ","))))
  
  # Reference ND catchments
  non_network_divides = filter(ref_nl$catchments,
                              !ref_nl$catchments$FEATUREID %in% u_fl) %>%
    select(ID = FEATUREID) %>%
    rename_geometry("geometry") %>%
    st_transform(st_crs(out_nl$catchments)) %>%
    rename(id = ID) %>%
    mutate(areasqkm = add_areasqkm(.),
           type     = ifelse(id < 0, "internal", "coastal"),
           toid = id) %>%
    rename_geometry("geometry") %>% 
    filter(!id %in% out_nl$catchments$id)
  
  hyaggregate_log("INFO", glue("{nrow(non_network_divides)} non network divides found"), verbose)
  
  divides = out_nl$catchments %>%
    mutate(type = "network") %>%
    select(id, toid, areasqkm, type) %>%
    rename_geometry("geometry") 
  
  if(nrow(non_network_divides) > 0){
    divides = bind_rows(divides, non_network_divides) 
  } 
  
  divdes = clean_geometry(divides, "id", keep = NULL, sys = FALSE) %>% 
    select(id, toid, areasqkm, type)
  
  write_sf(divides, gpkg, catchment_name, overwrite = TRUE)
  
  return(gpkg)

}