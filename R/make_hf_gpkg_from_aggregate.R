
#' Convert Aggregate Output to HF gpkg
#' This is a temporary function as changes get pushed upstream
#' @param gpkg gpkg file path
#' @return
#' @export
#' @examples

make_hf_gpkg_from_aggregate = function(gpkg){
  
  if (is.null(gpkg) | is.null(refactored_gpkg)) {
    stop("hydrofabrics must be provided.")
  }

  tmp = list()
  tmp$lookup = NULL

  nl$flowpaths = left_join(nl$flowpaths, select(st_drop_geometry(nl$catchments), id, divide_type), by = 
                         "id") %>% 
    mutate(network_type = ifelse(is.na(divide_type), "connector", divide_type), divide_type = NULL) 

  
  seed_network = rbind(select(st_drop_geometry(nl$flowpaths), id, toid, network_type),
        select(st_drop_geometry(nl$catchments), id, toid, network_type = divide_type)) %>% 
    filter(!duplicated(.)) %>% 
    mutate(has_flowline  = ifelse(network_type %in% c("coastal", "internal"), FALSE, TRUE),
           has_divide  = ifelse(network_type %in% c("connector"), FALSE, TRUE))
 
  
  member_ids = grep("member", names(nl$flowpaths), value = TRUE)
  
  if(length(member_ids) > 0 ){
    lookup  =  nl$flowpaths %>%
      st_drop_geometry() %>%
      select(
        id = id,
        toid          = toid,
        member  = !!member_ids,
        divide_id     = id,
        poi_id        = poi_id,
        mainstem = levelpathid,
        hydroseq = hydroseq,
        order = order) %>%
      mutate(member = strsplit(member, ","),
             poi_id = as.integer(poi_id)) %>%
      unnest(col = 'member') %>%
      mutate(hf_id_part = sapply( strsplit(member, "[.]"), FUN = function(x){ x[2] }),
             hf_id_part = ifelse(is.na(hf_id_part), 1L, as.integer(hf_id_part)),
             hf_id = sapply( strsplit(member, "[.]"), FUN = function(x){ as.numeric(x[1]) }),
             member = NULL,
             hf_source = "NHDPlusV2"
      ) %>% 
      select(id,hf_source, hf_id, hf_id_part, mainstem, divide_id, poi_id)
    
  } 
  
  if("poi_id" %in% names(nl$flowpaths)) {
    
   pois =  tryCatch({
      read_sf(gpkg, "mapped_POIs") %>% 
       rename_geometry("geometry") %>% 
       rename(poi_type = type, poi_value = value)
   }, error = function(e){
     NULL
   })
   
   tmp$POIs =  select(pois, poi_id, geometry) 
            
  }
  
  tmp$lookup_table = pois %>% 
    st_drop_geometry() %>% 
    right_join(lookup)
  
  # Flowpaths
  tmp$flowpaths = nl$flowpaths %>% 
    select(id, toid, mainstem = levelpathid, 
           lengthkm, areasqkm, tot_drainage_areasqkm, 
           order, hydroseq, has_divide) %>% 
    mutate(divide_id = id) %>% 
    rename_geometry("geometry")
  
  
  tmp$POIs = left_join(tmp$POIs, distinct(select(tmp$lookup_table, id, poi_id)), by = "poi_id")
  
  # Divides
  tmp$divides = nl$catchments %>% 
    select(divide_id = id, toid, areasqkm, network_type = divide_type) %>% 
    mutate(has_flowline = network_type == "network") %>% 
    left_join(select(st_drop_geometry(tmp$flowpaths), id, divide_id), by = "divide_id") %>% 
    rename_geometry("geometry")

  #  network

  tmp$network = seed_network %>% 
    mutate(divide_id = ifelse(network_type == "connector", NA, id)) %>% 
    left_join(select(st_drop_geometry(nl$flowpaths), id, poi_id, mainstem = levelpathid, 
                     lengthkm, areasqkm, 
                     tot_drainage_areasqkm)) 

  write_hydrofabric(tmp, gpkg, TRUE, enforce_dm = TRUE)

}
