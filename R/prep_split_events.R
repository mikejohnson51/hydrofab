#' Prep Split Events
#' @param pois a set of POIs with a poi_id, X and  (in 5070)
#' @param flines a set of flowlines geometries (EPSG:5070)
#' @param divides a set of divides geometries (EPSG:5070)
#' @param theshold a percentage (0-100) a POI must be upstream before splitting
#' @return sf POINT object
#' @export

prep_split_events = function(pois, fline, divides, threshold = 25) {

   if("id" %in% names(fline)){
     fline$COMID = fline$id
     fline$id = NULL
     flip_id = TRUE
   } else {
     flip_id = FALSE
   }
  
  if("frommeas" %in% names(fline)){
    fline$FromMeas = fline$frommeas
    fline$ToMeas = fline$tomeas
    flip_meas = TRUE
  } else {
    flip_meas = FALSE
  }
  
  if(inherits(pois, "sf")){
    pts =  st_filter(pois,divides)
  } else {
    pts = st_as_sf(pois, coords = c("X", "Y"), crs = 5070) |>
      st_filter(divides)
  }
   
  reference_poi = dplyr::bind_cols(pts, left_join(
    data.frame(id = seq_len(nrow(pts))),
    get_flowline_index(st_transform(fline, 5070), pts, search_radius = 50),
    by = "id" )) |>
    filter(!is.na(COMID)) |>
    st_sf() |>
    select(poi_id, COMID, REACHCODE, REACH_meas, offset) |>
    left_join(select(st_drop_geometry(fline), COMID, FromMeas, ToMeas), by = "COMID")
  
  split_sites <- reference_poi |>
    mutate(m = (100 * (REACH_meas - FromMeas) / (ToMeas - FromMeas))) |> 
    filter(m < threshold)
  
  if(flip_id){
    split_sites = rename(split_sites, id = COMID)
  } 
  
  if(flip_meas){
    split_sites = rename(split_sites, frommeas =  FromMeas, tomeas = ToMeas)
  } else {
    split_sites
  }
}
