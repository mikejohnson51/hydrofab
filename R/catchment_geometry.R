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

#' Fast Polygon Union
#' This is significantly faster then sf::st_union or summarize
#' @param poly sf POLYGON object
#' @param ID the column name over which to union geometries
#' @return sf object
#' @export
#' @importFrom sf as_Spatial st_as_sf st_cast st_make_valid
#' @importFrom dplyr mutate 
#' @importFrom rgeos gUnaryUnion

union_polygons_geos = function(poly, ID){
  
  SPDF =  as_Spatial(poly)
  
  rownames(SPDF@data) <- sapply(slot(SPDF, "polygons"), function(x) slot(x, "ID"))
  
  tmp <- gUnaryUnion(spgeom = SPDF, id = poly[[ID]], checkValidity = 2) 
  
  ids <- as.numeric(sapply(slot(tmp, "polygons"), function(x) slot(x, "ID")))
  
  suppressWarnings({
    st_as_sf(tmp) %>%
      mutate("{ID}" := ids) %>%
      mutate(areasqkm = add_areasqkm(.)) %>% 
      st_cast("POLYGON") %>% 
      st_make_valid()
  })
}

#' Catchment Geometry Doctor
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
#' @return sf object
#' @export
#' @importFrom dplyr select mutate filter group_by ungroup slice_max bind_rows n right_join rename slice_min
#' @importFrom sf st_crs st_transform st_area st_make_valid st_intersection st_collection_extract st_cast st_intersects st_length st_filter
#' @importFrom rmapshaper ms_explode ms_dissolve ms_simplify
#' @importFrom nhdplusTools rename_geometry


clean_geometry = function(catchments,
                                     ID = "ID",
                                     keep = .9) {
  in_crs = st_crs(catchments)
  
  in_cat <- suppressWarnings({
    catchments %>%
    dplyr::select(ID = !!ID) %>%
    st_transform(5070) %>%
    mutate(areasqkm = add_areasqkm(.)) %>% 
    ms_explode() %>%
    filter(!duplicated(.)) %>%
    mutate(area = add_areasqkm(.))
  })
  
  ids <- filter(in_cat, duplicated(ID))$ID
  
  if (length(ids) != 0) {
    
    # single geometry  catchments
    cat_no_problem <- filter(in_cat, !ID %in% ids)
    
    # multi geometry catchments
    challenges = filter(in_cat, ID %in% ids) %>%
      mutate(tmpID = 1:n())
    
    # base cats: combo of biggest multi geom and single geom catchments
    base_cats = challenges %>%
      group_by(ID) %>%
      slice_max(area) %>%
      ungroup() %>% 
      bind_rows(cat_no_problem) 
    
    # Frags are polygon slivers not in the base catchments
    frags = filter(challenges, !tmpID %in% base_cats$tmpID) %>%
      ms_dissolve() %>%
      ms_explode() %>%
      mutate(area = as.numeric(st_area(.))) %>%
      st_make_valid()
    
    # message(prettyNum(nrow(frags), big.mark = ",", scientific = FALSE), " fragments to clean...")
    
    out = tryCatch({
      suppressWarnings({
        st_intersection(frags, st_make_valid(base_cats)) %>%
          st_collection_extract("LINESTRING") 
      })
    }, error = function(e) { NULL })
    
    
    if(!is.null(out)){
      # ints are the LINSTRINGS of intersection between base_cats and frags
      ints = out %>%
        mutate(l = st_length(.)) %>%
        group_by(rmapshaperid) %>%
        slice_max(l, with_ties = FALSE) %>%
        ungroup()
      
      tj = right_join(frags,
                      dplyr::select(st_drop_geometry(ints), ID, rmapshaperid),
                      by = "rmapshaperid") %>%
        bind_rows(base_cats) %>%
        dplyr::select(-rmapshaperid, -areasqkm, -tmpID) %>%
        group_by(ID) %>%
        mutate(n = n()) %>%
        ungroup() %>% 
        nhdplusTools::rename_geometry('geometry') %>%
        ungroup()
      
      in_cat = suppressWarnings({
        union_polygons_geos(filter(tj, n > 1) , 'ID') %>%
          bind_rows(dplyr::select(filter(tj, n == 1), ID)) %>%
          mutate(tmpID = 1:n())
      })
      
    } else {
      in_cat = base_cats
    }
    
    # More then one polygon represents a FP
    dups = in_cat$ID[duplicated(in_cat$ID)]
    
    if (length(dups) > 0) {
      
      for (i in 1:length(dups)) {
        here = filter(in_cat, ID == dups[i]) 
        
        dissolve     =  slice_min(here, areasqkm, with_ties = FALSE)
        
        tmap = st_filter(in_cat, dissolve, .predicate = st_touches)
        
        opt = suppressWarnings({ 
          st_intersection(dissolve, filter(tmap, !tmpID %in% dissolve$tmpID)) %>%
            st_collection_extract("LINESTRING") %>%
            mutate(l = st_length(.)) %>%
            slice_max(l, with_ties = FALSE)
        })
        
        ind = which(in_cat$tmpID == opt$tmpID.1)
        
        in_cat$geometry[ind] = st_union(dissolve$geometry, in_cat$geometry[ind])
        in_cat = filter(in_cat, !tmpID %in%  dissolve$tmpID)
      }
    }
  }
  
  if (!is.null(keep)) {
    # message("Simplifying catchment boundaries: keep = ", keep)
    in_cat = ms_simplify(in_cat, keep = keep, keep_shapes = TRUE)
  }
  
  in_cat %>%
    mutate(areasqkm = add_areasqkm(.), tmpID = NULL) %>%
    st_transform(in_crs) %>%
    select("{ID}" := ID, areasqkm)  %>% 
    left_join(st_drop_geometry(catchments), by = "ID")
}


#' Add Length Map to Refactored Network
#' @description This function replicated the member_COMID column of a refactored 
#' network but adds a new notion. Following each COMID is '.' which is proceeded 
#' by the fraction of that COMID making up the new flowpath. For example 101.1 
#' would indicate 100% of COMID 101 is in the new ID. 
#' Equally 101.05 would indicate 50% of COMID 101 is present in the new ID
#' @param flowpaths a refactored flowpath network containing an member_COMID column
#' @return sf object
#' @export
#' @examples
#' path <- system.file("extdata/walker_reconcile.gpkg", package = "hyRefactor")
#' fps  <- add_lengthmap(sf::read_sf(path))
#' 
#'@importFrom dplyr select mutate filter left_join right_join arrange group_by summarize
#'@importFrom tidyr unnest
#'@importFrom sf st_drop_geometry st_length st_as_sf
#'@importFrom nhdplusTools get_vaa

add_lengthmap = function(flowpaths){
  
  unnested = dplyr::select(st_drop_geometry(flowpaths), 
                           ID, 
                           COMID = member_COMID) %>%
    mutate(COMID = strsplit(COMID, ",")) %>%
    tidyr::unnest(cols = COMID)
  
  unnested2 = filter(unnested, grepl("\\.", COMID)) %>% 
    mutate(baseCOMID = floor(as.numeric(COMID)))
  
  lengthm_fp = nhdplusTools::get_vaa(atts = "lengthkm") %>% 
    select(baseCOMID = comid, lengthkm) %>% 
    mutate(lengthm_fp = lengthkm * 1000, lengthkm = NULL) 
  
  lengthm_id = flowpaths %>% 
    mutate(lengthm_id = as.numeric(st_length(.))) %>% 
    select(ID, lengthm_id) %>% 
    st_drop_geometry()
  
  map = left_join(left_join(unnested2, lengthm_fp), lengthm_id) %>% 
    mutate(perLength = round(lengthm_id/lengthm_fp, 3)/10) %>% 
    select(ID, COMID, perLength) %>% 
    right_join(unnested) %>% 
    mutate(perLength = ifelse(is.na(perLength), 1, as.character(perLength))) %>% 
    arrange(ID) %>% 
    mutate(new = paste0(floor(as.numeric(COMID)), ".", gsub("0\\.", "", perLength))) %>% 
    group_by(ID) %>% 
    summarize(lengthMap = paste(new, collapse = ",")) %>% 
    right_join(flowpaths) %>% 
    st_as_sf()
  
  return(map)
}
