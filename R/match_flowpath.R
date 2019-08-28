#' @importFrom sf st_sf st_coordinates st_as_sf st_crs
#' @importFrom dplyr left_join select filter group_by ungroup bind_cols
#' @importFrom nhdplusTools prepare_nhdplus
mr_hw_cat_out <- function(mr_fline) {
  mr_fline <- prepare_nhdplus(mr_fline, 0, 0, 0, warn = FALSE) %>%
    left_join(select(mr_fline, COMID), by = "COMID") %>%
    st_sf() %>%
    filter(!COMID %in% toCOMID)

  outlets <-  mr_fline %>%
    st_coordinates() %>%
    as.data.frame() %>%
    group_by(L2) %>%
    filter(row_number() == round(n()/2)) %>%
    ungroup() %>%
    select(X, Y) %>%
    st_as_sf(coords = c("X", "Y"))

  bind_cols(outlets, select(st_set_geometry(mr_fline, NULL), COMID)) %>%
    st_sf(crs = st_crs(mr_fline))
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
#' For preformance reasons, network navigation only considers the network of levelpaths.
#' As a result, the response includes all flowlines along headwater levelpaths,
#' including those upstream of the outlet of the headwater catchment.
#'
#' @param source_flowline sf data.frame with source flowlines and flowline attributes:
#' COMID, LENGTHKM, DnHydroseq, and Hydroseq, and LevelPathI or NHDPlusHR equivalents.
#' @param target_catchment sf data.frame with catchment polygons and FEATUREID or
#' NHDPlusHR equivalent.
#' @param target_flowline sf data.frame with target flowlines and COMID or NHDPlusHR
#' equivalent.
#' @param hr_pair (advanced use) data.frame as output by get_hr_pair internal function.
#' @export
#' @importFrom sf st_join st_set_geometry st_within
#' @importFrom tidyr unnest
#' @importFrom dplyr select distinct  left_join bind_rows
#' @importFrom nhdplusTools get_DM
#' @examples
#' source(system.file("extdata/nhdplushr_data.R", package = "hyRefactor"))
#' source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
#'
#' lp_df_df <- match_flowpaths(source_flowline = new_hope_flowline,
#'                            target_catchment = hr_catchment,
#'                            target_flowline = hr_flowline)
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
match_flowpaths <- function(source_flowline, target_catchment, target_flowline,
                            hr_pair = NULL) {

  source_flowline <- nhdplusTools:::rename_nhdplus(source_flowline)
  check_names(source_flowline, "match_flowpaths")

  required_names <- unique(c(get("match_flowpaths_attributes",
                                 hyRefactor_env),
                             get("prepare_nhdplus_attributes",
                                 hyRefactor_env)))

  source_flowline <- select(source_flowline, required_names)

  target_flowline <- clean_geom(target_flowline)
  
  target_flowline <- select(target_flowline, NHDPlusID, HydroSeq, DnHydroSeq, LevelPathI, DnLevelPat)

  if(is.null(hr_pair)) {
    target_catchment <- nhdplusTools:::rename_nhdplus(target_catchment)
    hr_pair <- get_hr_pair(mr_hw_cat_out(source_flowline), target_catchment)
  }

  source_flowline <- clean_geom(source_flowline)
  mr_lp <- distinct(select(source_flowline, COMID, LevelPathI))
  rm(source_flowline)

  gc()

  hr_pair <- rename(hr_pair, NHDPlusID = FEATUREID) %>%
    filter(NHDPlusID %in% target_flowline$NHDPlusID)
  
  dm_NHDPlusID <- lapply(hr_pair$NHDPlusID,
                         function(x, fa) get_DM(fa, x),
                         fa = target_flowline)
  
  target_flowline <- select(target_flowline, NHDPlusID, hr_LevelPathI = LevelPathI)

  # Expand into data.frame
  lp_df <- data.frame(headwater_COMID = hr_pair$COMID)
  lp_df["member_NHDPlusID"] <- list(dm_NHDPlusID)

  lp_df <- unnest(lp_df)

  # Get MR levelpaths for headwater COMIDs
  hr_pair <- left_join(hr_pair, 
                       rename(mr_lp, mr_LevelPathI = LevelPathI), by = "COMID")

  # Join so we have HR NHDPlusIDs found downstream of a given MR LevelPath headwater.
  lp_df <- left_join(lp_df, select(hr_pair, -NHDPlusID), 
                     by = c("headwater_COMID" = "COMID"))

  group_by(lp_df, member_NHDPlusID) %>% # Group by level paths present in HR.
    filter(mr_LevelPathI == min(mr_LevelPathI)) %>% # Filter so only one (largest) MR levelpath is linked to each HR path.
    ungroup() %>%
    left_join(target_flowline,
              by = c("member_NHDPlusID" = "NHDPlusID"))
}

get_hr_pair <- function(mr_hw_outlets, target_catchment) {
  ### Find HR catchment of outlet of headwater MR catchments
  hr_pair <- st_join(mr_hw_outlets,
                     select(target_catchment, FEATUREID),
                     join = st_within) %>%
    st_set_geometry(NULL)
}
