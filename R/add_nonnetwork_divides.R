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

add_nonnetwork_divides = function(gpkg = NULL,
                                  flowpath = NULL,
                                  divide = NULL,
                                  reference_gpkg,
                                  verbose = TRUE){
  
  ref_nl = read_hydrofabric(reference_gpkg,  verbose = FALSE)
  
  catchment_name = grep("divide|catchment", st_layers(gpkg)$name, value = TRUE)
  
  out_nl = read_hydrofabric(gpkg, verbose = FALSE)
  
  # Encapsulated Flows
  u_fl = unique(as.integer(unlist(strsplit(out_nl$flowpaths$member_comid, ","))))
  
  # Reference ND catchments
  non_network_divdes = filter(ref_nl$catchments,
                              !ref_nl$catchments$FEATUREID %in% u_fl) %>%
    select(ID = FEATUREID) %>%
    rename_geometry("geometry") %>%
    st_transform(st_crs(out_nl$catchments)) %>%
    rename(id = ID) %>%
    mutate(areasqkm = add_areasqkm(.),
           type     = ifelse(id < 0, "internal", "coastal")) %>%
    rename_geometry("geometry")
  
  out_nl$catchments %>%
    select(id, areasqkm) %>%
    mutate(type = "network") %>%
    rename_geometry("geometry") %>%
    bind_rows(non_network_divdes) %>% 
    write_sf(gpkg, catchment_name)
  
  return(gpkg)
  
  #sinks = ...
}