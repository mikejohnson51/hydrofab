hy_drop_geometry <- function(x) {
  if("sf" %in% class(x)) {
    sf::st_drop_geometry(x)
  } else {
    x
  } 
}

#' Map outlets from COMID to ID for aggregate catchments
#' @description given reconciled flowlines and a set of source outlets,
#' returns a set of outlets with reconciled IDs suitable for use with 
#' aggregate_catchments.
#' @param source_outlets data.frame with COMID and type columns
#' @param reconciled data.frame as returned by refactor workflow
#' @importFrom dplyr select mutate
#' @export
map_outlet_ids <- function(source_outlets, reconciled) {

  source_outlets <- hy_drop_geometry(source_outlets)
  reconciled <- hy_drop_geometry(reconciled)
  
  # Convert member comid to a list column and truncate with as.integer
  # 1.1 becomes 1, 1.9 becomes 1, etc. This removes part ids.
  reconciled$integer_COMID <- strsplit(reconciled$member_COMID, ",")
  reconciled$integer_COMID <- lapply(reconciled$integer_COMID, as.integer)
  
  rec <- unnest_flines(select(reconciled, ID, integer_COMID), col = "integer_COMID")
  
  # finds IDs from reconciled that represent the passed-in COMID
  # returns IDs in the hyRefactor ID space
  find_ID <- function(COMID, rec) rec$ID[which(COMID == rec$integer_COMID)]
  
  # NOT COMID
  ids <- lapply(source_outlets$COMID, 
                FUN = find_ID, 
                rec = rec)
  
  id_selector <- lengths(ids) > 0
  
  source_outlets <- source_outlets[id_selector, ]  
  source_outlets$ID <- as.integer(lapply(ids[id_selector], function(x) x[[1]]))
  
  terminals <- reconciled[is.na(reconciled$toID), ]
  
  missing_terminals <- terminals[!terminals$ID %in% source_outlets$ID, ]
  
  if(nrow(missing_terminals) > 0) {
    missing_terminals <- select(missing_terminals, ID, COMID = member_COMID) %>%
      mutate(COMID = strsplit(COMID, split = ",")) %>%
      mutate(COMID = sapply(COMID, utils::tail, n = 1), type = "terminal")
    
    outlets <- rbind(source_outlets, missing_terminals)
  } else {
    outlets <- source_outlets
  }
  
  terminal_flowpath <- reconciled[which(reconciled$ID %in% source_outlets[source_outlets$type == "terminal", ]$ID), ]
  remove_terminal_flowpath <- terminal_flowpath[!is.na(terminal_flowpath$toID), ]
  
  outlets[!outlets$ID %in% remove_terminal_flowpath$ID, ]
}
