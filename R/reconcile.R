#' @title Reconcile Collapsed Flowlines
#' @description Reconciles output of collapse_flowlines giving a unique ID to
#' each new flowpath and providing a mapping to NHDPlus COMIDs.
#' @param flines data.frame with COMID, toCOMID, LENGTHKM, LevelPathI, Hydroseq,
#' and TotDASqKM columns
#' @param geom sf data.frame for flines
#' @param id character id collumn name.
#' @return reconciled flowpaths with new ID, toID, LevelPathID, and Hydroseq identifiers.
#' Note that all the identifiers are new integer IDs. LevelPathID and Hydroseq are consistent
#' with the LevelPathID and Hydroseq from the input NHDPlus flowlines.
#' @importFrom dplyr group_by ungroup filter left_join select rename
#' mutate distinct summarise arrange desc
#' @seealso The \code{\link{refactor_nhdplus}} function implements a complete
#' workflow using `reconcile_collapsed_flowlines()`.
#' @export
#'
reconcile_collapsed_flowlines <- function(flines, geom = NULL, id = "COMID") {

  check_names(flines, "reconcile_collapsed_flowlines")

  new_flines <-
    mutate(flines,
           becomes =
             ifelse((is.na(joined_fromCOMID) | joined_fromCOMID == -9999),
                     ifelse((is.na(joined_toCOMID) | joined_toCOMID == -9999),
                             COMID, joined_toCOMID),
                     joined_fromCOMID)) %>%
    group_by(becomes) %>%
    mutate(LENGTHKM = max(LENGTHKM),
           Hydroseq = min(Hydroseq),
           LevelPathI = min(LevelPathI)) %>%
    ungroup() %>%
    tidyr::separate(COMID, c("orig_COMID", "part"),
                    sep = "\\.", remove = FALSE, fill = "right") %>%
    mutate(new_Hydroseq = ifelse(is.na(part),
                                 as.character(Hydroseq),
                                 paste(as.character(Hydroseq),
                                       part, sep = ".")),
           part = ifelse(is.na(part), "0", part)) %>%
    ungroup() %>%
    select(-joined_fromCOMID, -joined_toCOMID)

  new_flines <-
    left_join(new_flines,
              data.frame(becomes = unique(new_flines$becomes),
                         ID = seq_len(length(unique(new_flines$becomes))),
                         stringsAsFactors = FALSE),
              by = "becomes")

  tocomid_updater <- filter(select(new_flines, becomes, toCOMID),
                            !is.na(toCOMID))

  new_flines <- distinct(left_join(select(new_flines, -toCOMID),
                                   tocomid_updater, by = "becomes"))

  new_flines <- left_join(new_flines,
                          select(new_flines, becomes, toID = ID),
                          by = c("toCOMID" = "becomes")) %>%
    arrange(Hydroseq, desc(part))

  new_flines <- left_join(new_flines,
                          data.frame(ID = unique(new_flines$ID),
                                     ID_Hydroseq = seq_len(length(unique(new_flines$ID)))),
                          by = "ID")

  new_lp <- group_by(new_flines, LevelPathI) %>%
    filter(Hydroseq == min(Hydroseq)) %>% # Get the outlet by hydrosequence.
    ungroup() %>%
    group_by(Hydroseq) %>%
    # Get the outlet if the original was split.
    filter(as.integer(part) == max(as.integer(part))) %>%
    ungroup() %>%
    select(ID_LevelPathID = ID_Hydroseq, LevelPathI)

  new_flines <- left_join(distinct(new_flines), distinct(new_lp), by = "LevelPathI") %>%
    select(ID, toID, LENGTHKM, TotDASqKM, member_COMID = COMID,
           LevelPathID = ID_LevelPathID, Hydroseq = ID_Hydroseq)

  if (!is.null(geom)) {
    geom_column <- attr(geom, "sf_column")

    if (is.null(geom_column)) stop("geom must contain an sf geometry column")

    new_flines <- left_join(new_flines, select(geom, id, geom_column),
                            by = c("member_COMID" = "COMID")) %>%
      sf::st_as_sf() %>%
      group_by(ID) %>%
      summarise(toID = toID[1],
                LENGTHKM = LENGTHKM[1],
                TotDASqKM = max(TotDASqKM),
                LevelPathID = LevelPathID[1],
                Hydroseq = Hydroseq[1],
                member_COMID = list(unique(member_COMID))) %>%
      sf::st_cast("MULTILINESTRING") %>%
      ungroup() %>%
      sf::st_line_merge()
  }
  return(new_flines)
}

#' @title Reconcile Catchment Divides
#' @description Reconciles catchment divides according to the output of
#' \code{\link{reconcile_collapsed_flowlines}} and \code{\link{refactor_nhdplus}}
#' @param fline_ref sf data.frame flowlines as returned by
#' \code{\link{refactor_nhdplus}} and \code{\link{reconcile_collapsed_flowlines}}
#' @param fline_rec sf data.frame flowpaths as returned by
#' \code{\link{reconcile_collapsed_flowlines}}
#' @param catchment sf data.frame NHDPlus Catchment or CatchmentSP layers for
#' included COMIDs
#' @param fdr raster D8 flow direction
#' @param fac raster flow accumulation
#' @param para integer numer of cores to use for parallel execution
#' @param cache path .rda to cache incremental outputs
#' @return Catchment divides that have been split and collapsed according to
#' input flowpaths
#' @seealso The \code{\link{refactor_nhdplus}} function implements a complete
#' workflow using `reconcile_collapsed_flowlines()` and can be used in prep
#' for this function.
#' @details Note that all inputs must be passed in the same projection.
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom sf st_drop_geometry st_dimension st_as_sf st_cast
#' @importFrom dplyr select filter mutate left_join
#' @importFrom data.table rbindlist
#'
reconcile_catchment_divides <- function(catchment, fline_ref, fline_rec, fdr, fac, para = 2, cache = NULL) {

  # This is a hack until I find time to get the geometry name dynamically.
  catchment <- rename_sf(catchment, "geom")
  fline_ref <- rename_sf(fline_ref, "geom")
  fline_rec <- rename_sf(fline_rec, "geom")
  
  check_proj(catchment, fline_ref, fdr)

  reconciled <- st_drop_geometry(fline_rec) %>%
    select(ID, member_COMID)

  rm(fline_rec)

  # Not all catchments have flowlines. Remove the flowlines without.  
  comid <- fline_ref$COMID # Just being explicit here.
  featureid <- catchment$FEATUREID # type conversion below is annoying.
  # as.integer removes the .1, .2, semantic part but the response retains 
  # the semantic component. If you don't know what this means, stop hacking.
  comid_with_catchment <- comid[as.integer(comid) %in% featureid]

  reconciled <- distinct(reconciled) %>% # had dups from prior steps.
    tidyr::separate_rows(member_COMID, sep = ",") %>% # Make long form
    dplyr::filter(member_COMID %in% comid_with_catchment) %>% # 
    dplyr::group_by(ID) %>%
    dplyr::summarise(member_COMID = stringr::str_c(member_COMID, collapse = ","))

  fline_ref <- fline_ref[as.integer(fline_ref$COMID) %in% catchment$FEATUREID, ]

  to_split_bool <- as.numeric(fline_ref$COMID) !=
    as.integer(fline_ref$COMID)

  to_split_ids <- fline_ref$COMID[which(to_split_bool)]

  to_split_featureids <- unique(as.integer(to_split_ids))

  cl <- NULL
  if(para > 1)
    cl <- parallel::makeCluster(rep("localhost", para),
                                type = "SOCK", outfile = "par_split.log")

  if(!is.null(cache)) {
    try(load(cache), silent = TRUE)
  }
  
  if(!exists("split_cats")) {
  split_cats <- pbapply::pblapply(to_split_featureids, par_split_cat,
                                    to_split_ids = to_split_ids,
                                    fline_ref = fline_ref,
                                    catchment = catchment,
                                    fdr = fdr, fac = fac, 
                                  cl = cl)
  if(!is.null(cache)) save(split_cats, file = cache)
  }

  split_cats <- st_as_sf(rbindlist(split_cats[!sapply(split_cats, is.null)])) %>%
    st_cast("MULTIPOLYGON")
  
  split_cats <- st_as_sf(rbindlist(list(
    get_cat_unsplit(catchment, fline_ref, to_split_featureids),
    split_cats)))

  combinations <- reconciled$member_COMID[grepl(",", reconciled$member_COMID)]
  
  unioned_cats <- pbapply::pblapply(combinations, 
                                    get_split_cats,
                                    split_cats = split_cats, 
                                    cl = cl)
  
  if(!is.null(cl))
    parallel::stopCluster(cl)
  
  if(length(unioned_cats) > 0) {
    unioned_cats <- st_as_sf(rbindlist(unioned_cats))
    split_cats <- st_as_sf(rbindlist(list(
      filter(split_cats, !FEATUREID %in% unioned_cats$FEATUREID),
      unioned_cats)))
  }

  out <- st_sf(left_join(reconciled, split_cats,
                         by = c("member_COMID" = "FEATUREID")))

  missing <- is.na(st_dimension(out$geom))

  if (any(missing)) {
    replace_cat <- select(catchment, member_COMID = FEATUREID) %>%
      filter(member_COMID %in% unique(as.integer(out$member_COMID[missing]))) %>%
      mutate(member_COMID = paste0(member_COMID, ".1")) %>%
      mutate(ID = out$ID[match(member_COMID, out$member_COMID)]) %>%
      select(ID, member_COMID)

    out <- filter(out, !missing) %>%
      st_cast("MULTIPOLYGON")

    names(replace_cat)[which(names(replace_cat) ==
                               attr(replace_cat,
                                    "sf_column"))] <- attr(out,
                                                           "sf_column")
    attr(replace_cat, "sf_column") <- attr(out, "sf_column")

    return(rbind(out, replace_cat))
  } else {
    return(out)
  }
}

par_split_cat <- function(fid, to_split_ids, fline_ref, catchment, fdr, fac) {
  out <- NULL
  try({
    # nolint start
    library(hyRefactor)
    # nolint end
    split_set <- to_split_ids[which(grepl(paste0("^", as.character(fid)), to_split_ids))]
    to_split_flines <- dplyr::filter(fline_ref, COMID %in% split_set)
    to_split_cat <- dplyr::filter(catchment, FEATUREID == fid)
    
    split_cats <- split_catchment_divide(catchment = to_split_cat,
                                         fline = to_split_flines,
                                         fdr = fdr,
                                         fac = fac)
    
    out <- sf::st_sf(FEATUREID = to_split_flines$COMID,
                     geom = sf::st_cast(split_cats, "MULTIPOLYGON"), 
                                        stringsAsFactors = FALSE)
  }
  , silent = FALSE)
  return(out)
}

get_split_cats <- function(cats, split_cats) {
  cats_vec <- unlist(strsplit(cats, ","))
  
  union_cats <- dplyr::filter(split_cats, FEATUREID %in% cats_vec)
  
  if (nrow(union_cats) != length(cats_vec)) {
    browser()
    stop(paste("missing a split catchment for an expected flowline.", 
               cats_vec))
  }
  
  unioned <- sf::st_sf(FEATUREID = cats, 
                       geom = sf::st_cast(sf::st_union(union_cats), 
                                          "MULTIPOLYGON"),
                       stringsAsFactors = FALSE)
  
  if (!any(grepl("MULTIPOLYGON", class(sf::st_geometry(unioned))))) {
    warning("Somthing is wrong with the union of catchments.")
  }
  
  return(unioned)
}

rename_sf <- function(sf_df, geom_name) {
  old_geom_name <- attr(sf_df, "sf_column")
  names(sf_df)[names(sf_df) == old_geom_name] <- geom_name
  
  attr(sf_df, "sf_column") <- geom_name
  
  sf_df
}

get_cat_unsplit <- function(catchment, fline_ref, to_split_featureids) {
  cat <- select(catchment, FEATUREID)
  cat <- cat[!cat$FEATUREID %in% to_split_featureids, ]
  cat$FEATUREID <- as.character(cat$FEATUREID)
  cat <- left_join(cat,
                        select(st_drop_geometry(fline_ref), 
                               FEATUREID = COMID),
                        by = "FEATUREID")
  st_cast(select(cat, FEATUREID), "MULTIPOLYGON")
}
