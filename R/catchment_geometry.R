#' Compute km2 area
#' Short hand for safely computing area in sqkm and returning as numeric vector.
#' @param x sf object
#' @return numeric vector
#' @export
#' @examples
#' library(sf)
#' nc = st_read(system.file("shape/nc.shp", package="sf"))
#' add_areasqkm(nc[1,])
#' @importFrom units set_units drop_units

add_areasqkm = function(x){
  drop_units(set_units(st_area(x), "km2"))
}

#' Fast POLYGON Union
#' @description This is significantly faster then sf::st_union or summarize
#' @param poly sf POLYGON object
#' @param ID the column name over which to union geometries
#' @return sf object
#' @export
#' @importFrom terra aggregate vect makeValid
#' @importFrom dplyr select
#' @importFrom sf st_as_sf st_collection_extract st_geometry_type st_make_valid

union_polygons = function(poly, ID){
  
  poly = select(poly, !!ID) %>% 
    vect() %>% 
    makeValid() %>%
    aggregate(by = eval(ID)) %>%
    st_as_sf() %>%
    select(!!ID)
  
  if (any(grepl("COLLECTION",  st_geometry_type(poly)))) {
    poly = st_collection_extract(poly, "POLYGON")
  }
  return(poly)

}

#' DEPRECATED: Fast LINESTRING union
#' @description Wayyyy faster then either data.table, or sf based line merging
#' @param lines lines to merge
#' @param ID ID to merge over
#' @return an sf object
#' @export

union_linestrings_geos = function(lines, ID){
  
  u <- union_linestrings(lines, ID)
  
  u[match(unique(lines[[ID]]), u[[ID]]), ]
  
}

#' Fast LINESTRING union
#' @description Wayyyy faster then either data.table, or sf based line merging
#' @param lines lines to merge
#' @param ID ID to merge over
#' @return an sf object
#' @export
#' @importFrom terra aggregate vect
#' @importFrom dplyr select
#' @importFrom sf st_as_sf

union_linestrings = function (lines, ID)  {
  aggregate(vect(lines), by = eval(ID)) %>%
    st_as_sf() %>%
    select(!!ID) %>%
    flowpaths_to_linestrings()
}

#' Convert MULITLINESTINGS to LINESTRINGS
#' @param flowpaths a flowpath `sf` object
#' @return a `sf` object
#' @export
#' @importFrom sf st_geometry_type st_geometry st_line_merge
#' @importFrom dplyr bind_rows

flowpaths_to_linestrings = function(flowpaths){
  bool = (st_geometry_type(sf::st_geometry(flowpaths)) == "MULTILINESTRING")
  multis = flowpaths[bool, ]
  if(nrow(multis) > 0){
    sf::st_geometry(multis) = st_line_merge(sf::st_geometry(multis))
  }
  singles = flowpaths[!bool, ]

  bind_rows(multis, singles)
}


#' Clean Catchment Geometry
#' @description Fixes geometry issues present in catchments that originate in the
#' CatchmentSP layers, or from the reconcile_catchments hyRefactor preocess.
#' These include, but are not limited to disjoint polygon fragments, artifacts
#' from the 30m DEM used to generate the catchments, and non-valid geometry topolgies.
#' A goal of this functions is also to provide means to reduce the data column
#' of the catchments by offering a topology preserving simplification
#' through \code{\link[rmapshaper]{ms_simplify}}.
#' Generally a "keep" parameter of .9 seems appropriate for the resolution of
#' the data but can be modified in function
#' @param catchments catchments geometries to fix
#' @param ID name of uniquely identifying column
#' @param keep proportion of points to retain in geometry simplification
#' (0-1; default 0.05). See \code{\link[rmapshaper]{ms_simplify}}.
#' If NULL, then no simplification will be executed.
#' @param crs integer or object compatible with sf::st_crs coordinate reference.
#' Should be a projection that supports area-calculations.
#' @param sys logical should the mapshaper system library be used. If NULL 
#' the system library will be used if available.
#' @return sf object
#' @export
#' @importFrom dplyr select mutate filter group_by ungroup slice_max bind_rows n right_join rename slice_min
#' @importFrom sf st_crs st_touches st_transform st_area st_make_valid st_intersection st_collection_extract st_cast st_intersects st_length st_filter st_union st_is_empty
#' @importFrom rmapshaper ms_explode ms_dissolve ms_simplify
#' @importFrom nhdplusTools rename_geometry
#' @importFrom rlang :=

clean_geometry <- function(catchments,
                          ID = "ID",
                          keep = .9,
                          crs = 5070, 
                          sys = NULL) {

  if(is.null(sys)) {
    sys <- FALSE
    try(sys <- is.character(rmapshaper::check_sys_mapshaper(verbose = FALSE)))
  }
  
  in_crs = st_crs(catchments)

  in_cat <- suppressWarnings({
    catchments %>%
    dplyr::select(ID = !!ID) %>%
    st_transform(crs) %>%
    mutate(areasqkm = add_areasqkm(.)) %>%
    ms_explode(sys = sys) %>%
    filter(!duplicated(.)) %>%
    mutate(area = add_areasqkm(.))
  })

  ids <- unique(filter(in_cat, duplicated(.data$ID))$ID)

  if (length(ids) != 0) {

    # single geometry catchments
    cat_no_problem <- filter(in_cat, !.data$ID %in% ids)

    # multi geometry catchments
    challenges = filter(in_cat, .data$ID %in% ids) %>%
      mutate(tmpID = 1:n()) %>%
      filter(!st_is_empty(.))

    # base cats: combo of biggest multi geom and single geom catchments
    base_cats = challenges %>%
      group_by(.data$ID) %>%
      slice_max(.data$area) %>%
      ungroup() %>%
      bind_rows(cat_no_problem)

    # Frags are polygon slivers not in the base catchments
    frags = filter(challenges, !.data$tmpID %in% base_cats$tmpID)

    # frags smaller than a square meter can be dropped.
    frags <- filter(frags, area > 0.000001)

    # If fragments exist, then fix, else return empty data.frame
    if(nrow(frags) > 0){
      frags = frags %>%
        ms_dissolve(sys = sys) %>%
        ms_explode(sys = sys) %>%
        mutate(area = as.numeric(st_area(.))) %>%
        st_make_valid()

      out = tryCatch({
        suppressWarnings({
          st_intersection(frags, st_make_valid(base_cats)) %>%
            st_collection_extract("LINESTRING")
        })
      }, error = function(e) { NULL })

    } else {
      out = data.frame()
    }

    if(is.null(out)){
      in_cat = base_cats
    } else if(!is.null(out) & nrow(out) != 0){
      # ints are the LINSTRINGS of intersection between base_cats and frags
      ints = out %>%
        mutate(l = st_length(.)) %>%
        group_by(.data$rmapshaperid) %>%
        slice_max(.data$l, with_ties = FALSE) %>%
        ungroup()

      tj = right_join(frags,
                      dplyr::select(st_drop_geometry(ints), .data$ID,
                                    .data$rmapshaperid),
                      by = "rmapshaperid") %>%
        bind_rows(base_cats) %>%
        dplyr::select(-.data$rmapshaperid, -.data$areasqkm, -.data$tmpID) %>%
        group_by(.data$ID) %>%
        mutate(n = n()) %>%
        ungroup() %>%
        nhdplusTools::rename_geometry('geometry') %>%
        ungroup()

      if(nrow(tj) == 0){
        in_cat = base_cats
      } else {
        in_cat = suppressWarnings({
          union_polygons(filter(tj, .data$n > 1) , 'ID') %>%
            bind_rows(dplyr::select(filter(tj, .data$n == 1), .data$ID)) %>%
            mutate(tmpID = 1:n())
        })
      }

    } else {
      in_cat = base_cats
    }

    # More then one polygon represents a FP
    dups = in_cat$ID[duplicated(in_cat$ID)]

    if (length(dups) > 0) {

      for (i in 1:length(dups)) {
        here = filter(in_cat, .data$ID == dups[i])

        dissolve = slice_min(here, .data$areasqkm, with_ties = FALSE)

        tmap = st_filter(in_cat, dissolve, .predicate = st_touches)

        suppressWarnings({
          opt <- st_intersection(dissolve, filter(tmap, !.data$tmpID %in% dissolve$tmpID))

          # If the intersection is only multipoint geometry
          # Note that geometry_type will be longer than one if more than one feature
          # was returned from the st_intersection above.
          gt <- sf::st_geometry_type(opt)
          if(length(gt) == 1 && grepl("POINT", gt)) {
            opt <- sf::st_cast(opt, "LINESTRING")
          }

          opt <- opt %>%
            st_collection_extract("LINESTRING") %>%
            mutate(l = st_length(.)) %>%
            slice_max(.data$l, with_ties = FALSE)

        })

        ind = which(in_cat$tmpID == opt$tmpID.1)

        in_cat$geometry[ind] = st_union(dissolve$geometry, in_cat$geometry[ind])
        in_cat = filter(in_cat, !.data$tmpID %in%  dissolve$tmpID)
      }
    }
  }
  

  if (!is.null(keep)) {
    # message("Simplifying catchment boundaries: keep = ", keep)
    if("tmpID" %in% names(in_cat)){
      in_cat$tmpID = NULL
    }
    
    in_cat = ms_simplify(in_cat, keep = keep, keep_shapes = TRUE, sys = sys)
  }

  # since areasqkm will be added based on the new geometries,
  # we need to drop that column if it exists. We want to retain
  # any others that that came with the OG catchments

  if('areasqkm' %in% names(catchments)){
    catchments = select(catchments, -.data$areasqkm)
  }

  in_cat %>%
    mutate(areasqkm = add_areasqkm(.)) %>%
    st_transform(in_crs) %>%
    dplyr::select("{ID}" := ID, .data$areasqkm)  %>%
    left_join(st_drop_geometry(catchments), by = ID) %>%
    filter(!duplicated(.))

}

#' Add Length Map to Refactored Network
#' @description This function replicates the member_COMID column of a refactored
#' network but adds a new notation Following each COMID is '.' which is proceeded
#' by the fraction of that COMID making up the new flowpath. For example 101.1
#' would indicate 100% of COMID 101 is in the new ID.
#' Equally 101.05 would indicate 50% of COMID 101 is present in the new ID'd flowpath
#' @param flowpaths a refactored flowpath network containing an member_COMID column
#' @param length_table a table of NHDPlus COMIDs and LENGTH to use as weights.
#' Can be found with \code{nhdplusTools::get_vaa("lengthkm")}
#' @return sf object
#' @export
#' @examples
#' \dontrun{
#' path <- system.file("extdata/walker_reconcile.gpkg", package = "hyRefactor")
#' fps  <- add_lengthmap(flowpaths = sf::read_sf(path),
#' length_table = nhdplusTools::get_vaa("lengthkm"))
#' }
#'@importFrom dplyr select mutate filter left_join right_join arrange group_by summarize
#'@importFrom sf st_drop_geometry st_length st_as_sf
#'@importFrom nhdplusTools get_vaa

add_lengthmap = function(flowpaths, length_table){

  tmp = dplyr::select(st_drop_geometry(flowpaths),
                           .data$ID,
                           COMID = .data$member_COMID) %>%
    mutate(COMID = strsplit(.data$COMID, ","))

  unnested = data.frame(ID =  rep(tmp$ID,  times = lengths(tmp$COMID)),
                    COMID = as.character(unlist(tmp$COMID)))

  unnested2 = filter(unnested, grepl("\\.", COMID)) %>%
    mutate(baseCOMID = floor(as.numeric(COMID)))

  lengthm_fp = length_table %>%
    select(baseCOMID = .data$comid, .data$lengthkm) %>%
    mutate(lengthm_fp = .data$lengthkm * 1000, lengthkm = NULL)
  
  lengthm_id = flowpaths %>%
    mutate(lengthm_id = as.numeric(st_length(.))) %>%
    select(.data$ID, .data$lengthm_id) %>%
    st_drop_geometry()

  map = left_join(left_join(unnested2, lengthm_fp, "baseCOMID"), lengthm_id, by = "ID") %>%
    mutate(perLength = round(.data$lengthm_id/.data$lengthm_fp, 3)/10) %>%
    select(.data$ID, .data$COMID, .data$perLength) %>%
    right_join(unnested, by = c("ID", "COMID")) %>%
    mutate(perLength = ifelse(is.na(.data$perLength), 1,
                              as.character(.data$perLength))) %>%
    arrange(.data$ID) %>%
    mutate(new = paste0(floor(as.numeric(.data$COMID)), ".",
                        gsub("0\\.", "", .data$perLength))) %>%
    group_by(.data$ID) %>%
    summarize(lengthMap = paste(.data$new, collapse = ",")) %>%
    right_join(flowpaths) %>%
    st_as_sf()

  return(map)
}
