#' Add small area to crosswalk
#' @param crosswalk an existing crosswalk table
#' @param comid the shared ID
#' @return data.frame
#' @export

add_areasqkm_to_crosswalk = function(crosswalk, comid = "hf_id"){
  get_vaa('areasqkm') %>% 
    select(s_areasqkm = areasqkm, !!comid := comid) %>% 
    right_join(crosswalk, by = eval(comid)) 
}


names <- c("nhdplus_comid","model_name","g_file","last_modified","source","units","crs","initial_scrape_name","final_name_key","notes")

ras_catalog_dbase <- data.frame(matrix(1:40, ncol = 10, nrow = 4))
colnames(ras_catalog_dbase) <- names
ras_catalog_dbase <- data.table::as.data.table(ras_catalog_dbase)
ras_catalog_dbase[,nhdplus_comid := as.character(nhdplus_comid)]

class(ras_catalog_dbase$nhdplus_comid)
