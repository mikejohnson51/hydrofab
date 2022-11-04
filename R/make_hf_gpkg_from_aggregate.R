#' Convert Aggregate Output to HF gpkg
#' This is a temporary function as changes get pushed upstream
#' @param gpkg gpkg file path
#' @return file.path
#' @export

make_hf_gpkg_from_aggregate = function(gpkg){
  
  if (is.null(gpkg) | is.null(refactored_gpkg)) {
    stop("hydrofabrics must be provided.")
  }

  nl = list()
  lyrs = st_layers(gpkg)$name
 
  for(i in 1:length(lyrs)){
    nl[[lyrs[i]]] = read_sf(gpkg, lyrs[i])
  }

  nl$flowpaths = left_join(nl$flowpaths, 
                           select(st_drop_geometry(nl$catchments), id, divide_type), 
                           by = "id") %>% 
    mutate(network_type = ifelse(is.na(divide_type), "connector", divide_type), divide_type = NULL) 

  
  seed_network = rbind(select(st_drop_geometry(nl$flowpaths), id, toid, network_type),
        select(st_drop_geometry(nl$catchments), id, toid, network_type = divide_type)) %>% 
    filter(!duplicated(.)) %>% 
    mutate(has_flowline  = ifelse(network_type %in% c("coastal", "internal"), FALSE, TRUE),
           has_divide  = ifelse(network_type %in% c("connector"), FALSE, TRUE))
 
  member_ids = grep("member", names(nl$flowpaths), value = TRUE)
  
  if(length(member_ids) > 0 ){
    nl$network_lookup  =  nl$flowpaths %>%
      st_drop_geometry() %>%
      select(
        id = id,
        toid          = toid,
        member  = !!member_ids,
        divide_id     = id,
        hl_id        = hl_id,
        mainstem = levelpathid,
        hydroseq = hydroseq,
        order = order) %>%
      mutate(member = strsplit(member, ","),
             hl_id = as.integer(hl_id)) %>%
      unnest(col = 'member') %>%
      mutate(hf_id_part = sapply( strsplit(member, "[.]"), FUN = function(x){ x[2] }),
             hf_id_part = ifelse(is.na(hf_id_part), 1L, as.integer(hf_id_part)),
             hf_id = sapply( strsplit(member, "[.]"), FUN = function(x){ as.numeric(x[1]) }),
             member = NULL,
             hf_source = "NHDPlusV2"
      ) %>% 
      select(id,hf_source, hf_id, hf_id_part, mainstem, divide_id, hl_id)
  } 
  
  
  #  network
  nl$network = seed_network %>% 
    mutate(divide_id = ifelse(network_type == "connector", NA, id)) %>% 
    left_join(select(st_drop_geometry(nl$flowpaths), id, hl_id, mainstem = levelpathid, 
                     lengthkm, areasqkm, 
                     tot_drainage_areasqkm), by = "id") 
  
  # Flowpaths
  nl$flowpaths = nl$flowpaths %>% 
    select(id, toid, mainstem = levelpathid, 
           lengthkm, areasqkm, tot_drainage_areasqkm, 
           order, hydroseq, has_divide, hl_id) %>% 
    mutate(divide_id = id) %>% 
    rename_geometry("geometry")
  
  nl$hydrolocations = rename_geometry(nl$hydrolocations, "geometry")

  # Divides
  nl$divides = nl$catchments %>% 
    select(divide_id = id, toid, areasqkm, network_type = divide_type) %>% 
    mutate(has_flowline = network_type == "network") %>% 
    left_join(select(st_drop_geometry(nl$flowpaths), id, divide_id), by = "divide_id") %>% 
    rename_geometry("geometry")

  nl$catchments = NULL
  
  write_hydrofabric(nl, gpkg, TRUE, enforce_dm = TRUE)

}
