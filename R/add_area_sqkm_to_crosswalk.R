#' Add small area to crosswalk
#' @param crosswalk an existing crosswalk table
#' @param comid the shared ID
#' @return data.frame
#' @importFrom nhdplusTools get_vaa
#' @importFrom dplyr select right_join
#' @export

add_areasqkm_to_crosswalk = function(crosswalk, comid = "hf_id"){
  get_vaa('areasqkm') %>% 
    select(s_areasqkm = areasqkm, !!comid := comid) %>% 
    right_join(crosswalk, by = eval(comid)) 
}