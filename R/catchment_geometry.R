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
#' @importFrom terra aggregate vect
#' @importFrom dplyr select
#' @importFrom sf st_as_sf

union_linestrings = function (lines, ID)  {
  aggregate(vect(lines), by = eval(ID)) %>%
    st_as_sf() %>%
    select(!!ID) %>%
    flowpaths_to_linestrings()
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
#' CatchmentSP layers, or from the reconcile_catchments hydrofab preocess.
#' These include, but are not limited to disjoint polygon fragments, artifacts
#' from the 30m DEM used to generate the catchments, and non-valid geometry topolgies.
#' A goal of this functions is also to provide means to reduce the data column
#' of the catchments by offering a topology preserving simplification
#' through \code{\link[rmapshaper]{ms_simplify}}.
#' Generally a "keep" parameter of .9 seems appropriate for the resolution of
#' the data but can be modified in function
#' @param catchments catchments geometries to fix
#' @param flowlines flowlines geometries to filter largest unit (optional)
#' @param ID name of uniquely identifying column
#' @param fl_ID flowlines unique identifier
#' @param keep proportion of points to retain in geometry simplification
#' (0-1; default 0.05). See \code{\link[rmapshaper]{ms_simplify}}.
#' If NULL, then no simplification will be executed.
#' @param crs integer or object compatible with sf::st_crs coordinate reference.
#' Should be a projection that supports area-calculations.
#' @param sys logical should the mapshaper system library be used. If NULL 
#' the system library will be used if available.
#' @return sf object
#' @export

clean_geometry <- function(catchments,
                           flowlines = NULL,
                           fl_ID = NULL,
                           ID = "ID",
                           keep = NULL,
                           crs = 5070,
                           grid = .0009,
                           sys = NULL) {
  
  # keep an original count of # rows in catchments
  MASTER_COUNT = nrow(catchments)
  
  # use system mapshaper or not
  if(Sys.getenv("TURN_OFF_SYS_MAPSHAPER") == "YUP")  { sys <- FALSE }
  
  if(is.null(sys)) {
    sys <- FALSE
    try(sys <- is.character(check_sys_mapshaper(verbose = FALSE)))
  }
  
  # set crs variable to crs of catchments
  if(!is.null(crs)){  crs = st_crs(catchments)  }
  
  catchments = lwgeom::st_snap_to_grid(st_transform(catchments, 5070), size = grid)
  
  # cast MPs to POLYGONS and add featureid count column
  polygons = suppressWarnings({
    catchments %>% 
      st_cast("MULTIPOLYGON") %>% 
      st_cast("POLYGON") %>% 
      fast_validity_check() %>% 
      add_count(!!sym(ID)) %>% 
      mutate(areasqkm = add_areasqkm(.), tmpID = 1:n()) %>% 
      rename_geometry("geometry") 
  })
  
  if(any(st_geometry_type(polygons) != "POLYGON") | 
     nrow(polygons) != MASTER_COUNT){
    # separate polygons with more than 1 feature counts
    extra_parts = filter(polygons, n != 1) 
    
    # dissolve, and explode if necessary
    try(
      extra_parts <- ms_explode(ms_dissolve(extra_parts, ID, copy_fields = names(extra_parts), sys = sys), sys = sys), 
      silent = TRUE
    )
    
    extra_parts$tmpID = 1:nrow(extra_parts)
    
    if(!is.null(flowlines)){
      
      fl = filter(flowlines, .data[[fl_ID]] %in% extra_parts[[ID]])
      
      imap = st_intersects(extra_parts, st_transform(st_zm(fl), st_crs(extra_parts)))
  
      l = lengths(imap)
    
      df = data.frame(
        tmpID = rep(extra_parts[['tmpID']], times = l),
        uid = rep(extra_parts[[ID]], times = l),
        touch_id = fl[[fl_ID]][unlist(imap)]
      ) %>% 
        group_by(tmpID) %>% 
        summarize(prime = any(uid == touch_id))
      
      extra_parts = left_join(extra_parts, df, by = 'tmpID') %>% 
        mutate(prime = ifelse(is.na(prime), FALSE, prime))
      
    } else {
      extra_parts$prime = FALSE
    }
    
    # recalculate area
    extra_parts <- mutate(extra_parts, 
                          areasqkm = add_areasqkm(extra_parts)) %>% 
      arrange(desc(prime), desc(areasqkm)) %>% 
      mutate(newID = row_number())

    # get the biggest parts by area in each catchment and bind with rest of good_to_go catchments
    main_parts <- 
      extra_parts %>% 
      group_by(.data[[ID]]) %>% 
      slice(1) %>% 
      ungroup() 
    
    small_parts <- 
      extra_parts %>% 
      filter(!newID %in% main_parts$newID)
    
    if(!sum(nrow(main_parts)) + nrow(filter(polygons, n == 1)) == MASTER_COUNT){ stop() }
    
    main_parts =  bind_rows(main_parts, filter(polygons, n == 1))
    
    if(is.null(small_parts)){
      small_parts = data.frame()
    }
    
    if(nrow(small_parts) > 0){
      # dissolve, and explode if necessary
      small_parts <- tryCatch(
        ms_explode(ms_dissolve(small_parts, ID, copy_fields = names(small_parts), sys = sys), sys = sys), 
        error = function(e){ NULL}, 
        warning = function(w){ NULL }
      )
      
      # add area
      small_parts =  mutate(
        small_parts, 
        areasqkm = add_areasqkm(small_parts),
        newID    = 1:n()
      ) %>% 
        select(newID)
      
      # get the intersection between big parts and small parts and pull out the LINESTRINGs
      out = tryCatch({
        suppressWarnings({
          st_intersection(small_parts, st_make_valid(main_parts)) %>%
            st_collection_extract("LINESTRING")
        })
      }, error = function(e) {
        st_intersection(small_parts, st_make_valid(main_parts)) %>%
          st_collection_extract("POINT")
      })
  
      
      ints =  out %>%
        mutate(l = st_length(.)) %>%
        group_by(newID) %>%
        slice_max(l, with_ties = FALSE) %>%
        ungroup()
      
      tj = right_join(
        small_parts,
        select(st_drop_geometry(ints), !!ID, newID),
        by = "newID"
      ) %>%
        bind_rows(main_parts) %>%
        select(-areasqkm, -tmpID, -newID) %>%
        group_by(.data[[ID]]) %>%
        mutate(n = n()) %>%
        ungroup() %>%
        rename_geometry('geometry')
      
      in_cat <- 
        union_polygons(
          filter(tj, .data$n > 1),
          ID
        ) %>% 
        bind_rows(
          select(
            filter(tj, .data$n == 1), 
            !!ID)
        ) %>%
        mutate(tmpID = 1:n()) %>% 
        fast_validity_check()
      
    } else {
      in_cat = fast_validity_check(main_parts)
    }

    if(!is.null(keep)){ 
      in_cat = simplify_process(in_cat, keep, sys)
    } 
  
    
    return(
     mutate(in_cat, areasqkm = add_areasqkm(in_cat)) %>%
        st_transform(crs) %>%
        select("{ID}" := ID, areasqkm)  %>%
        left_join(st_drop_geometry(select(catchments, -any_of('areasqkm'))), by = ID)
    )
    
  } else {
    
    if(!is.null(keep)){ 
      polygons = simplify_process(polygons, keep, sys) %>% 
            fast_validity_check()
    } 
    
     return(
       select(polygons, -n, -tmpID)
     )
  }
}
  
# quickly check and validate invalid geometries only
fast_validity_check <- function(x){
  
  bool    = st_is_valid(x)
  valid   = filter(x, bool)
  invalid = st_make_valid(filter(x, !bool)) %>% 
    st_cast("POLYGON")
  
  return(bind_rows(valid, invalid))
  
}

simplify_process = function(catchments, keep, sys){
  
  # simplify catchments
  catchments =  ms_simplify(catchments, 
                            keep = keep,
                            keep_shapes = TRUE, 
                            sys = sys)
  
  tt = fast_validity_check(catchments)
  
  if(nrow(tt) != sum(st_is_valid(tt))){
    stop("Invalid simplication")
  } else {
    tt
  }
  # # mark valid/invalid geoms
  # bool = st_is_valid(catchments)
  # 
  # # make invalid geoms valid
  # invalids = st_make_valid(filter(catchments, !bool))
  # 
  # # if not all polygons get returned, try different simplification keep value
  # if(nrow(filter(invalids, st_geometry_type(invalids) != "POLYGON")) > 0){
  #   
  #   warning("Invalid geometries found. Trying new keep of:",  keep + ((1-keep) / 2) , call. = FALSE)
  #   
  #   # try simplification again
  #   catchments = ms_simplify(catchments, keep =  keep + ((1-keep) / 2), keep_shapes = TRUE, sys = sys)
  #   
  #   # mark valid/invalid geoms
  #   bool = st_is_valid(catchments)
  #   
  #   # make invalid geoms valid
  #   invalids = st_make_valid(filter(catchments, !bool))
  #   
  #   # if catchments still containing non POLYGON geometries, return original data
  #   if(nrow(filter(invalids, st_geometry_type(invalids) != "POLYGON")) > 0){ 
  #     
  #     warning("Invalid geometries found. Original catchments returned." , call. = FALSE) 
  #     
  #     return(catchments)
  #     
  #   } else {
  #     
  #     # combine corrected invalids with valids and recalc area
  #     return(
  #       mutate(
  #         bind_rows(invalids, filter(catchments, bool)), 
  #         areasqkm = add_areasqkm(.)
  #       ) 
  #     )
  #   }
  #   
  # } else {
  #   
  #   # combine corrected invalids with valids and recalc area
  #   return(
  #     bind_rows(invalids, filter(catchments, bool)) %>% 
  #       mutate( areasqkm = add_areasqkm(.)) 
  #   )
  # }
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
#' path <- system.file("extdata/walker_reconcile.gpkg", package = "hydrofab")
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
