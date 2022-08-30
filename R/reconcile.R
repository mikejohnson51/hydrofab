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
             ifelse((is.na(.data$joined_fromCOMID) | .data$joined_fromCOMID == -9999),
                     ifelse((is.na(.data$joined_toCOMID) | .data$joined_toCOMID == -9999),
                            .data$COMID, .data$joined_toCOMID),
                    .data$joined_fromCOMID))
  
  # In the case that something is first joined to then the thing it joins with joins from
  # we have to do a little reassignment.
  joined_from <- new_flines[new_flines$joined_fromCOMID %in% new_flines$becomes, ]
  joined_to   <- new_flines[new_flines$joined_toCOMID %in% new_flines$becomes, ]
  
  joined_tofrom <- joined_to[joined_to$becomes %in% joined_from$COMID, ]
  
  update_tofrom <- left_join(select(joined_tofrom, .data$COMID, .data$becomes), 
                             select(joined_from, .data$COMID, new_becomes = .data$becomes), 
                             by = c("becomes" = "COMID"))
  
  if(nrow(update_tofrom) > 0) {
    new_flines <- left_join(new_flines, select(update_tofrom, .data$COMID, .data$new_becomes), by = "COMID") %>%
      mutate(becomes = ifelse(!is.na(.data$new_becomes), .data$new_becomes, .data$becomes)) %>%
      select(-.data$new_becomes)
  }
  
  new_flines <- new_flines %>%
    group_by(.data$becomes) %>%
    mutate(LENGTHKM = max(.data$LENGTHKM),
           Hydroseq = min(.data$Hydroseq),
           LevelPathI = min(.data$LevelPathI)) %>%
    ungroup() %>%
    tidyr::separate(.data$COMID, c("orig_COMID", "part"),
                    sep = "\\.", remove = FALSE, fill = "right") %>%
    mutate(new_Hydroseq = ifelse(is.na(.data$part),
                                 as.character(.data$Hydroseq),
                                 paste(as.character(.data$Hydroseq),
                                       part, sep = ".")),
           part = ifelse(is.na(.data$part), "0", part)) %>%
    ungroup() %>%
    select(-.data$joined_fromCOMID, -.data$joined_toCOMID)

  new_flines <-
    left_join(new_flines,
              data.frame(becomes = unique(new_flines$becomes),
                         ID = seq_len(length(unique(new_flines$becomes))),
                         stringsAsFactors = FALSE),
              by = "becomes")
  
  tocomid_updater <- filter(select(new_flines, .data$becomes, .data$toCOMID),
                            !is.na(.data$toCOMID))

  new_flines <- distinct(left_join(select(new_flines, -.data$toCOMID),
                                   tocomid_updater, by = "becomes"))

  new_flines <- left_join(new_flines,
                          select(new_flines, .data$becomes, toID = .data$ID),
                          by = c("toCOMID" = "becomes")) %>%
    arrange(.data$Hydroseq, desc(.data$part))

  new_flines <- left_join(new_flines,
                          data.frame(ID = unique(new_flines$ID),
                                     ID_Hydroseq = seq_len(length(unique(new_flines$ID)))),
                          by = "ID")

  new_lp <- group_by(new_flines, .data$LevelPathI) %>%
    filter(Hydroseq == min(.data$Hydroseq)) %>% # Get the outlet by hydrosequence.
    ungroup() %>%
    group_by(.data$Hydroseq) %>%
    # Get the outlet if the original was split.
    filter(as.integer(.data$part) == max(as.integer(.data$part))) %>%
    ungroup() %>%
    select(ID_LevelPathID = .data$ID_Hydroseq, .data$LevelPathI)
  
  if(!"event_identifier" %in% names(new_flines)) 
    new_flines$event_identifier <- rep(NA, nrow(new_flines))

  new_flines <- left_join(distinct(new_flines), distinct(new_lp), by = "LevelPathI") %>%
    select(.data$ID, .data$toID, .data$LENGTHKM, .data$TotDASqKM, member_COMID = .data$COMID,
           LevelPathID = .data$ID_LevelPathID, Hydroseq = .data$ID_Hydroseq, 
           .data$event_identifier, orig_levelpathID = .data$LevelPathI)

  if (!is.null(geom)) {
    geom_column <- attr(geom, "sf_column")

    if (is.null(geom_column)) stop("geom must contain an sf geometry column")

    new_flines <- right_join(select(geom, member_COMID = id, geom_column), 
                             new_flines,
                             by = "member_COMID")
    
    new_flines <- new_flines %>%
      drop_geometry() %>%
      group_by(.data$ID) %>%
      summarise(toID = .data$toID[1],
                LENGTHKM = .data$LENGTHKM[1],
                TotDASqKM = max(.data$TotDASqKM),
                LevelPathID = .data$LevelPathID[1],
                Hydroseq = .data$Hydroseq[1],
                event_identifier = .data$event_identifier[1],
                orig_levelpathID = .data$orig_levelpathID[1],
                member_COMID = list(unique(.data$member_COMID))) %>%
      ungroup() %>%
      left_join(union_linestrings(
        select(new_flines[!sf::st_is_empty(new_flines), ], .data$ID), "ID"),
        by = "ID") %>%
      sf::st_as_sf()
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
#' @param fdr character path to D8 flow direction
#' @param fac character path to flow accumulation
#' @param para integer numer of cores to use for parallel execution
#' @param cache path .rda to cache incremental outputs
#' @param fix_catchments logical. should catchment geometries be rectified?
#' @param keep Only applicable if fix_catchments = TRUE. Defines the proportion 
#' of points to retain in geometry simplification (0-1; default 0.05). 
#' See \code{\link[rmapshaper]{ms_simplify}}. Set to NULL to skip
#' simplification.
#' @param vector_crs integer or object compatible with sf::st_crs coordinate reference.
#' Should be a projection that supports area-calculations.
#' @inheritParams split_catchment_divide
#' @return Catchment divides that have been split and collapsed according to
#' input flowpaths
#' @seealso The \code{\link{refactor_nhdplus}} function implements a complete
#' workflow using `reconcile_collapsed_flowlines()` and can be used in prep
#' for this function.
#' @details Note that all inputs must be passed in the same projection.
#' @export
#' @importFrom sf st_crs st_transform st_precision st_drop_geometry st_sf st_multipolygon st_as_sf st_cast st_dimension
#' @importFrom nhdplusTools rename_geometry
#' @importFrom terra crs res
#' @importFrom dplyr select distinct filter group_by summarise ungroup row_number mutate bind_rows
#' @importFrom tidyr separate_rows
#' @importFrom parallel makeCluster stopCluster
#' @importFrom pbapply pblapply
#' @importFrom data.table rbindlist

reconcile_catchment_divides <- function(catchment, 
                                              fline_ref, 
                                              fline_rec, 
                                              fdr = NULL, fac = NULL,
                                              para = 2, cache = NULL, 
                                              min_area_m = 800, 
                                              snap_distance_m = 100,
                                              simplify_tolerance_m = 40, 
                                              vector_crs = 5070,
                                              fix_catchments = TRUE,
                                              keep = .9) {
  
  in_crs    <- st_crs(catchment)
  catchment <- rename_geometry(catchment, "geom")
  fline_ref <- rename_geometry(fline_ref, "geom")
  fline_rec <- rename_geometry(fline_rec, "geom")
  
  if(!is.null(fdr) & !is.null(fac)){
    
    fdr_temp <- fdr
    if(!inherits(fdr_temp, "SpatRaster")){
      fdr_temp <- terra::rast(fdr_temp)
    }

    catchment <-  st_transform(catchment, terra::crs(fdr_temp)) 
    #st_precision(catchment) <- terra::res(fdr_temp)[1]
    fline_ref <-  st_transform(fline_ref,  terra::crs(fdr_temp))
    fline_rec <-  st_transform(fline_rec,  terra::crs(fdr_temp))
  }
  
  reconciled <- st_drop_geometry(fline_rec) %>%
    dplyr::select(.data$ID, .data$member_COMID)
  
  rm(fline_rec)
  
  # Not all catchments have flowlines. Remove the flowlines without.  
  comid <- fline_ref$COMID # Just being explicit here.
  featureid <- catchment$FEATUREID # type conversion below is annoying.
  # as.integer removes the .1, .2, semantic part but the response retains 
  # the semantic component. If you don't know what this means, stop hacking.
  comid_with_catchment <- comid[as.integer(comid) %in% featureid]
  
  reconciled <- distinct(reconciled) %>% # had dups from prior steps.
    tidyr::separate_rows(.data$member_COMID, sep = ",") %>% # Make long form
    dplyr::filter(.data$member_COMID %in% comid_with_catchment) %>% # 
    dplyr::group_by(.data$ID) %>%
    dplyr::summarise(member_COMID = paste(.data$member_COMID, collapse = ",")) %>%
    dplyr::ungroup()

  fline_ref <- fline_ref[as.integer(fline_ref$COMID) %in% catchment$FEATUREID, ]
  
  to_split_bool <- as.numeric(fline_ref$COMID) !=
    as.integer(fline_ref$COMID)
  
  to_split_ids <- fline_ref$COMID[which(to_split_bool)]
  
  to_split_featureids <- unique(as.integer(to_split_ids))
  
  cl <- NULL
  
  if(para > 1) {
    log_file <- ""
    if(!is.null(cache)) log_file <- paste0(cache, "_par.log")  
    cl <- parallel::makeCluster(para, outfile = log_file)
  }
  
  if(!is.null(cache)) {
    try(load(cache), silent = TRUE)
  }
  
  if(!exists("split_cats")) {
    split_cats <- pbapply::pblapply(to_split_featureids, 
                                    par_split_cat,
                                    to_split_ids = to_split_ids,
                                    fline_ref = fline_ref,
                                    catchment = catchment,
                                    fdr = fdr, 
                                    fac = fac,
                                    min_area_m = min_area_m, 
                                    snap_distance_m = snap_distance_m,
                                    simplify_tolerance_m = simplify_tolerance_m, 
                                    vector_crs = vector_crs,
                                    cl = cl)
    if(!is.null(cache)) save(split_cats, file = cache)
  }
  
  
  if(length(split_cats) == 0) {
    split_cats <- st_sf(FEATUREID = NA, geom = list(sf::st_multipolygon()))
  } else {
    if(!inherits(split_cats, "sf")) {
      split_cats <- sf::st_as_sf(data.table::rbindlist(split_cats[!sapply(split_cats, is.null)]))
    }
    split_cats <- sf::st_cast(split_cats, "MULTIPOLYGON")
  }
  
  split_cats <- dplyr::group_by(split_cats, FEATUREID) %>% 
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup()
  
  unsplit <- get_cat_unsplit(catchment, fline_ref, to_split_featureids)
  
  if(nrow(unsplit) > 0) {
    split_cats <- sf::st_as_sf(data.table::rbindlist(list(
      unsplit,
      split_cats)))
  }
  
  combinations <- reconciled$member_COMID[grepl(",", reconciled$member_COMID)]
  
  unioned_cats <- pbapply::pblapply(combinations, 
                                    get_split_cats,
                                    split_cats = split_cats,
                                    cache = cache,
                                    cl = cl)


  if(!is.null(cl)) {
    parallel::stopCluster(cl)
  }
  
  if(length(unioned_cats) > 0) {
    unioned_cats <- sf::st_as_sf(data.table::rbindlist(unioned_cats))
    split_cats   <- sf::st_as_sf(data.table::rbindlist(list(
      dplyr::filter(split_cats, !FEATUREID %in% unioned_cats$FEATUREID),
      unioned_cats
    )))
  }
  
  out <- st_sf(right_join(dplyr::select(split_cats, member_COMID = .data$FEATUREID), 
                          reconciled,
                          by = "member_COMID"))
  
  missing <- is.na(st_dimension(out$geom))
  
  if (any(missing)) {
    
    out_mp <- filter(out, !missing) %>%
      st_cast("MULTIPOLYGON")
    
    out <- select(catchment, member_COMID = .data$FEATUREID) %>%
      filter(.data$member_COMID %in% unique(as.integer(out$member_COMID[missing]))) %>%
      mutate(member_COMID = paste0(.data$member_COMID, ".1")) %>%
      mutate(ID = out$ID[match(.data$member_COMID, out$member_COMID)]) %>%
      select(.data$ID, .data$member_COMID) %>% 
      nhdplusTools::rename_geometry(attr(out_mp, "sf_column")) %>% 
      bind_rows(out_mp)
  } 
  
  if(fix_catchments){
    # cat("Fixing Catchment Geometries...\n")
    clean_geometry(catchments = out, "ID", keep) %>% 
      sf::st_transform(in_crs)
  } else {
    sf::st_transform(out, in_crs)
  }
}

par_split_cat <- function(fid, to_split_ids, fline_ref, catchment, fdr, fac,
                          min_area_m, snap_distance_m,
                          simplify_tolerance_m, vector_crs) {
  out <- NULL
  try({
    
    message(paste0(Sys.time(), " par_split_cat() on pid: ", 
                   Sys.getpid(), " catchment: ", fid))
    
    # nolint start
    requireNamespace("hyRefactor", quietly = TRUE)
    requireNamespace("terra", quietly = TRUE)
    # nolint end
    
    if(!inherits(fdr, "SpatRaster")){
      fdr = terra::rast(fdr)
    }
    
    if(!inherits(fac, "SpatRaster")){
      fac = terra::rast(fac)
    }
    
    split_set <- to_split_ids[which(grepl(paste0("^", as.character(fid)), to_split_ids))]
    to_split_flines <- dplyr::filter(fline_ref, .data$COMID %in% split_set)
    to_split_cat    <- dplyr::filter(catchment, .data$FEATUREID == fid)
    
    split_cats <- split_catchment_divide(catchment = to_split_cat,
                                         fline = to_split_flines,
                                         fdr = fdr,
                                         fac = fac, 
                                         lr = FALSE,
                                         min_area_m = min_area_m, 
                                         snap_distance_m = snap_distance_m,
                                         simplify_tolerance_m = simplify_tolerance_m, 
                                         vector_crs = vector_crs)
    
    out <- sf::st_sf(FEATUREID = to_split_flines$COMID,
                     geom = sf::st_cast(split_cats, "MULTIPOLYGON"), 
                                        stringsAsFactors = FALSE)
  }, silent = FALSE)
  
  return(out)
}

get_split_cats <- function(cats, split_cats, cache = NULL) {
  
  error_found <- TRUE
  error_file <- paste0(gsub(".rda", "", cache), "error.rda")
  
  tryCatch({
  cats_vec <- unlist(strsplit(cats, ","))
  
  union_cats <- dplyr::filter(split_cats, .data$FEATUREID %in% cats_vec)

  if (length(unique(union_cats$FEATUREID)) != length(cats_vec)) {
    if(!is.null(cache)) {
      
      save(list = ls(), file = error_file)
      stop(paste("missing a split catchment for an expected flowline.", 
                 cats_vec, "environment saved to:", cache))
    } 
    stop(paste("missing a split catchment for an expected flowline.", 
                paste(cats_vec, collapse = "\n")))

  }
  
  geom <- sf::st_cast(sf::st_union(sf::st_make_valid(union_cats)), 
                      "MULTIPOLYGON")
  
  unioned <- sf::st_sf(FEATUREID = cats, 
                       geom = geom,
                       stringsAsFactors = FALSE)
  
  if (!any(grepl("MULTIPOLYGON", class(sf::st_geometry(unioned))))) {
    warning("Somthing is wrong with the union of catchments.")
  }
  
  return(unioned)
  }, error = function(e) {
    warning(paste0("error in get_split_cats", e))
  })
  
  save(list = ls(), file = error_file)
  stop(paste("Something went wrong in get_split_cats. The environment was saved to", cache))
  
}


get_cat_unsplit <- function(catchment, fline_ref, to_split_featureids) {
  cat <- select(catchment, FEATUREID)
  cat <- cat[!cat$FEATUREID %in% to_split_featureids, ]
  cat$FEATUREID <- as.character(cat$FEATUREID)
  cat <- left_join(cat,
                        select(st_drop_geometry(fline_ref), 
                               FEATUREID = .data$COMID),
                        by = "FEATUREID")
  st_cast(select(cat, .data$FEATUREID), "MULTIPOLYGON")
}
