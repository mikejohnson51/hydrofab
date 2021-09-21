#nolint start
# NHDPlus Attributes
COMID <- FEATUREID <-
  Hydroseq <- DnHydroseq <- DnMinorHyd <- LevelPathI <- DnLevelPat <-
  ToNode <- FromNode <-
  TotDASqKM <- LENGTHKM <-
  Pathlength <- StreamCalc <- StreamOrde <- TerminalFl <-
  Divergence <- TerminalPa <- StartFlag <- FTYPE <-
  FromMeas <- ToMeas <- REACHCODE <- REACH_meas <-
  HUC12 <- TOHUC <- NULL

# Package Attribute Names
# Need to clean up.
COMID.y <- ID <- becomes <- ds_num_upstream <- fID <-
  dsLENGTHKM <- ds_joined_fromCOMID <- fromCOMID <-
  fromTotDASqKM <- geom_len <-
  geometry <- join_category <- joined_fromCOMID <-
  joined_fromCOMID_new <- joined_toCOMID <- member_COMID <-
  new_joined_fromCOMID <- new_joined_toCOMID <- new_toCOMID <-
  num_upstream <- part <- piece <- pieces <- removed_COMID <-
  split_fID <- toCOMID <- toID <- usTotDASqKM <-
  . <- L1 <- X <- Y <- breaks <- dist_ratio <- ideal_len <-
  len <- nID <- new_index <- piece_len <- setNames <- start <-
  index <- measure <- nn.idx <- precision_index <- max_Hydroseq <-
  nn.dists <- offset <- area <- member_FEATUREID <- geom <-
  fromID <- nexID <- cat_ID <- type <- LevelPathID <- orig_COMID <-
  tail_id <- tail_ID <- toID_hydroseq <- toID_tail_ID <- toID_fromID <-
  toID_LevelpathID <- set <- set_toID <- usLevelPathI <- fromLevelPathI <-
  ID_Hydroseq <- ID_LevelPath <- ID_LevelPathID <- toID_fromID_TotDASqKM <-
  toID_fromID_lp <- denTotalAreaSqKM <- check_LevelPathI <-
  correct_head_HUC12 <- corrected_LevelPathI <- head_HUC12 <-
  intersected_LevelPathI <- levelpath <- main_LevelPathI <- nameID <-
  nhd_LevelPath <- outletID <- outlet_HUC12 <- update_head_HUC12 <-
  updated_head_HUC12 <- updated_outlet_HUC12 <- weight <- hu12 <-
  lp <- L2 <- members <- DnHydroSeq <- HydroSeq <- NHDPlusID <-
  member_NHDPlusID <- mr_LevelPathI <- event_REACH_meas <- .data <- NULL

# nolint end
hyrefactor_env <- new.env()

assign("prepare_nhdplus_attributes",
       c("COMID", "LENGTHKM", "FTYPE", "TerminalFl",
         "FromNode", "ToNode", "TotDASqKM",
         "StartFlag", "StreamOrde", "StreamCalc",
         "TerminalPa", "Pathlength", "Divergence", "Hydroseq",
         "LevelPathI"),
       envir = hyrefactor_env)

assign("split_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM"),
       envir = hyrefactor_env)

assign("collapse_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM", "LevelPathI", "Hydroseq"),
       envir = hyrefactor_env)

assign("reconcile_collapsed_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM", "LevelPathI", "Hydroseq"),
       envir = hyrefactor_env)

assign("match_levelpaths_attributes",
       c("COMID", "Hydroseq", "LevelPathI",
         "DnLevelPat", "denTotalAreaSqKM", "HUC12", "TOHUC"),
       envir = hyrefactor_env)

assign("match_flowpaths_attributes",
       c("COMID", "LENGTHKM", "DnHydroseq",
         "Hydroseq", "LevelPathI", "DnLevelPat"),
       envir = hyrefactor_env)

assign("split_lines_event_attributes",
       c("REACHCODE", "FromMeas", "ToMeas"),
       envir = hyrefactor_env)

assign("aggregate_network_attributes",
       c("ID", "toID", "LevelPathID", "Hydroseq"),
       envir = hyrefactor_env)

check_names <- function(x, function_name) {
  x <- nhdplusTools::align_nhdplus_names(x)
  names_x <- names(x)

  expect_names <- get(paste0(function_name, "_attributes"),
                      envir = hyrefactor_env)

  if (!all(expect_names %in% names_x)) {
    stop(paste0("Missing some required attributes in call to: ",
                function_name, ". Expected: ",
                paste(expect_names[which(!(expect_names %in%
                                             names_x))],
                      collapse = ", "), "."))
  }
  return(invisible(x))
}

#nolint start
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(strwrap(
    "USGS Support Package:
    https://owi.usgs.gov/R/packages.html#support"),
    collapse = "\n"))
  invisible(NULL)
}
# nolint end

get_ds_lengthkm <- function(flines) {
  # This gets all the next-downstream flowlines and finds the
  # length of the next downstream
  flines[["dsLENGTHKM"]] <-
    flines[["LENGTHKM"]][match(flines$toCOMID, flines$COMID)]
  # already removed comids get NA dsLength -- ok to set them to 0.
  flines[["dsLENGTHKM"]][is.na(flines$dsLENGTHKM)] <- 0
  flines[["dsLENGTHKM"]]
}

get_upstream <- function(flines) {
  left_join(select(flines, COMID), select(flines, COMID, toCOMID),
            by = c("COMID" = "toCOMID")) %>%
    rename(fromCOMID = COMID.y)
}

get_num_upstream <- function(flines) {
  left_join(select(flines, COMID, toCOMID),
            get_upstream(flines) %>%
              group_by(COMID) %>%
              summarise(num_upstream = n()),
            by = "COMID")[["num_upstream"]]
}

get_ds_num_upstream <- function(flines) {
  flines <- mutate(flines, num_upstream = get_num_upstream(flines))
  flines[["num_upstream"]][match(flines$toCOMID, flines$COMID)]
}

get_ds_joined_fromcomid <- function(flines) {
  flines <- mutate(flines, ds_joined_fromCOMID = joined_fromCOMID)
  flines[["ds_joined_fromCOMID"]][match(flines$toCOMID, flines$COMID)]
}

drop_geometry <- function(x) {
  if("sf" %in% class(x)) {
    sf::st_drop_geometry(x)
  } else {
    x
  }
}