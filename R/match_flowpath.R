#' Get headwater points
#' @description Returns headwater locations for line geometries 
#' @param fline flowline data.frame from NHDPlus or with an ID and toID column.
#' @importFrom sf st_sf st_coordinates st_as_sf st_crs
#' @importFrom dplyr left_join select filter group_by ungroup bind_cols
#' @importFrom nhdplusTools prepare_nhdplus
#' @export
get_hw_points <- function(fline) {
  if("COMID" %in% names(fline)) {
    fline <- prepare_nhdplus(fline, 0, 0, 0, warn = FALSE) %>%
      left_join(select(fline, COMID), by = "COMID") %>%
      st_sf() %>%
      filter(!COMID %in% toCOMID)
  } else {
    fline <- filter(fline, !ID %in% toID)
  }

  outlets <-  fline %>%
    st_coordinates() %>%
    as.data.frame()
  
  if("L2" %in% names(outlets)) {
    outlets <- group_by(outlets, L2)
  } else {
    outlets <- group_by(outlets, L1)
  }
  
  outlets <- outlets %>%
    filter(row_number() == round(n()/2)) %>%
    ungroup() %>%
    select(X, Y) %>%
    st_as_sf(coords = c("X", "Y"))

  if("COMID" %in% names(fline)) {
    bind_cols(outlets, select(st_set_geometry(fline, NULL), COMID)) %>%
      st_sf(crs = st_crs(fline))
  } else {
    bind_cols(outlets, select(st_set_geometry(fline, NULL), ID)) %>%
      st_sf(crs = st_crs(fline))
  }
}

clean_geom <- function(x) {
  if("sf" %in% class(x)) {
    st_set_geometry(x, NULL)
  } else {
    x
  }
}

#' Match Flowpaths
#' @description Implements a flowpath-matching algorithm that traces downstream along
#' the target flowline network and determines which levelpath from the source flowlines
#' best matches the resulting downstream traces. The algorithm starts from the outlet
#' location of the upstream most catchment in the source flowlines to stay away from
#' complexity that occurs near drainage divides.
#'
#' @param source_flowline sf data.frame with COMID and LevelPathI attributes.
#' @param target_flowline sf data.frame either NHDPlusHR with NHDPlusID, LENGTHKM, DnHydroseq, 
#' Hydroseq, and LevelPathI or an ad-hoc network with ID and toID attributes.
#' @param hw_pair data.frame with headwater pairs
#' @return data.frame with the headwater id from the source_flowline input and 
#' levelpath from the source_flowline input for each identifier matched from the 
#' target dataset.
#' @export
#' @importFrom sf st_join st_set_geometry st_within
#' @importFrom tidyr unnest
#' @importFrom dplyr select distinct  left_join bind_rows
#' @importFrom nhdplusTools get_DM
#' @examples
#' source(system.file("extdata/nhdplushr_data.R", package = "hyRefactor"))
#' source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
#'
#' hr_catchment <- nhdplusTools:::rename_nhdplus(hr_catchment)
#' hw_pair <- sf::st_set_geometry(sf::st_join(get_hw_points(new_hope_flowline),
#'                               dplyr::select(hr_catchment, FEATUREID),
#'                               join = sf::st_within), NULL)
#'
#' lp_df_df <- match_flowpaths(new_hope_flowline, hr_flowline, hw_pair)
#' matched <- dplyr::left_join(dplyr::select(hr_flowline, NHDPlusID),
#'                             dplyr::select(lp_df_df, member_NHDPlusID,
#'                                           MR_LevelPathI = mr_LevelPathI), 
#'                                           by = c("NHDPlusID" = "member_NHDPlusID"))
#'
#' lp <- min(matched$MR_LevelPathI, na.rm = TRUE)
#' mr_lp <- dplyr::filter(new_hope_flowline, LevelPathI <= lp)
#' hr_lp <- dplyr::filter(matched, MR_LevelPathI <= lp)
#' plot(sf::st_geometry(matched), col = "blue", lwd = 0.5)
#' plot(sf::st_geometry(mr_lp), col = "red", lwd = 3, add = TRUE)
#' plot(sf::st_geometry(hr_lp), col = "black", add = TRUE)
#'
match_flowpaths <- function(source_flowline, target_flowline, hw_pair) {

  source_flowline <- nhdplusTools:::rename_nhdplus(source_flowline)
  source_flowline <- clean_geom(source_flowline)
  target_flowline <- clean_geom(target_flowline)
  
  if("ID" %in% names(target_flowline)) {
    
    lpt <- data.frame(headwater_COMID = hw_pair$FEATUREID)
    
    lpt["member_ID"] <- list(lapply(hw_pair$ID,
                                      function(x, fa) get_dwn(x, fa),
                                      fa = target_flowline))
    
    lpt <- unnest(lpt)
    
    lps <- data.frame(headwater_COMID = hw_pair$FEATUREID)
    
    lps["member_COMID"] <- list(lapply(hw_pair$FEATUREID,
                                             function(x, fa) get_DM(fa, x),
                                             fa = source_flowline))
    
    lps <- unnest(lps)
    
    lps <- lps %>%
      left_join(select(source_flowline, LevelPathI, Hydroseq, COMID), 
                by = c("member_COMID" = "COMID")) %>%
      group_by(LevelPathI) %>%
      filter(Hydroseq == max(Hydroseq)) %>% 
      ungroup() %>% group_by(headwater_COMID) %>%
      filter(LevelPathI == min(LevelPathI)) %>%
      select(-Hydroseq, mr_LevelPathI = LevelPathI, -member_COMID) %>%
      ungroup()
    
    target_flowline <- select(clean_geom(target_flowline), ID)
    source_flowline <- distinct(select(source_flowline, COMID, LevelPathI))
    
    gc()
    
    hw_pair <- hw_pair %>%
      rename(COMID = FEATUREID) %>%
      left_join(lps, by = c("COMID" = "headwater_COMID"))
    
    # Join so we have RF1 IDs found downstream of a given MR LevelPath headwater.
    lpt <- left_join(lpt, select(hw_pair, -ID),
                     by = c("headwater_COMID" = "COMID"))
    
    group_by(lpt, member_ID) %>%
      filter(mr_LevelPathI == min(mr_LevelPathI)) %>%
      ungroup()
      
  } else {

  target_flowline <- select(clean_geom(target_flowline),
                            NHDPlusID, HydroSeq, DnHydroSeq, 
                            LevelPathI, DnLevelPat)
  
  gc()

  hw_pair <- rename(hw_pair, NHDPlusID = FEATUREID) %>%
    filter(NHDPlusID %in% target_flowline$NHDPlusID)
  
  dm_NHDPlusID <- lapply(hw_pair$NHDPlusID,
                         function(x, fa) get_DM(fa, x),
                         fa = target_flowline)
  
  target_flowline <- select(target_flowline, NHDPlusID)

  # Expand into data.frame
  lp_df <- data.frame(headwater_COMID = hw_pair$COMID)
  lp_df["member_NHDPlusID"] <- list(dm_NHDPlusID)

  lp_df <- unnest(lp_df)

  # Get MR levelpaths for headwater COMIDs
  hw_pair <- left_join(hw_pair, 
                       rename(source_flowline, mr_LevelPathI = LevelPathI), 
                       by = "COMID")

  # Join so we have HR NHDPlusIDs found downstream of a given MR LevelPath headwater.
  lp_df <- left_join(lp_df, select(hw_pair, -NHDPlusID), 
                     by = c("headwater_COMID" = "COMID"))

  group_by(lp_df, member_NHDPlusID) %>% # Group by level paths present in HR.
    filter(mr_LevelPathI == min(mr_LevelPathI)) %>% # Filter so only one (largest) MR levelpath is linked to each HR path.
    ungroup() %>%
    left_join(target_flowline,
              by = c("member_NHDPlusID" = "NHDPlusID")) %>%
    select(-headwater_COMID)
  }
}

get_dwn_rec <- function(ID, target_flowline) {
  next_dn <- target_flowline$toID[target_flowline$ID == ID]
  if(is.na(next_dn)) return()
  c(ID, get_dwn(next_dn, target_flowline))
}

get_dwn <- function(ID, target_flowline) {
  next_dn <- target_flowline$toID[target_flowline$ID == ID]
  if(is.na(next_dn)) {
    return(ID)
  } else {
    return(get_dwn_rec(ID, target_flowline))
  }
}
