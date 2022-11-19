#' Convert Target Size Aggregate output to HF gpkg
#' This is a temporary function as changes get pushed upstream
#' @param gpkg gpkg file path
#' @return file.path
#' @export

make_hf_gpkg_from_uniform_aggregate = function(gpkg){
  
  if (is.null(gpkg)) {
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

#' Convert Reference Output to HF gpkg
#' This is a temporary function as changes get pushed upstream
#' @param gpkg gpkg file path
#' @return file.path
#' @export

make_hf_gpkg_from_reference = function(gpkg){
  
  if (is.null(gpkg)) {
    stop("hydrofabrics must be provided.")
  }
  
  nl = list()
  lyrs = st_layers(gpkg)$name
  
  flowpath_name = grep("flow", lyrs, value = TRUE)
  divides_name = grep("catchment|divide", lyrs, value = TRUE)
  wb_name = grep("WB", lyrs, value = TRUE)
  poi_name = grep("POI", lyrs, value = TRUE)

  # Missing areasqkm, divide_id, has_divide
  nl$flowpaths = read_sf(gpkg, flowpath_name) %>% 
    select(id = COMID, toid = toCOMID, mainstem = LevelPathI,
           lengthkm = LENGTHKM, tot_drainage_areasqkm = TotDASqKM,
           order = StreamOrde, hydroseq = Hydroseq, hl_id = POI_ID, wb_id = WB) %>% 
    mutate(wb_id = ifelse(wb_id == 0, NA, wb_id)) %>% 
    rename_geometry("geometry")
  
  # Missing network_type, id, toid, has_flowline
  nl$divides   = read_sf(gpkg, divides_name) %>% 
    select(divide_id = FEATUREID, areasqkm = AREASQKM) %>% 
    mutate(has_flowline = divide_id %in% nl$flowpaths$id,
           id = ifelse(divide_id %in% nl$flowpaths$id, divide_id, NA)) %>% 
    left_join(select(st_drop_geometry(nl$flowpaths), id, toid), by = "id") %>% 
    mutate(network_type = case_when(
      !has_flowline & divide_id < 0 ~ "internal",
      !has_flowline & divide_id > 0 ~ "coastal",
      has_flowline  ~ "network",
    )) %>% 
    rename_geometry("geometry")
  
  nl$flowpaths = nl$flowpaths %>% 
    mutate(has_divide = id %in% nl$divides$divide_id) %>% 
    left_join(select(st_drop_geometry(nl$divides), id, divide_id, areasqkm), by = "id") %>% 
    mutate(network_type = case_when(
      has_divide  ~ "network",
      !has_divide ~ "connector"
    ))
  
  nl$wbs = read_sf(gpkg, wb_name) %>% 
    select(wb_id = COMID, wb_area = LakeArea, RESOLUTION) %>% 
    mutate(wb_source = paste("NHDPlus", RESOLUTION), RESOLUTION = NULL)

  seed_network = rbind(select(st_drop_geometry(nl$flowpaths),  divide_id, id, toid, network_type),
                       select(st_drop_geometry(nl$divides),    divide_id, id, toid, network_type)) %>% 
    filter(!duplicated(.)) %>% 
    mutate(has_flowline  = ifelse(network_type %in% c("coastal", "internal"), FALSE, TRUE),
           has_divide  = ifelse(network_type %in% c("connector"), FALSE, TRUE))
  
  fl_net = st_drop_geometry(nl$flowpaths) %>% 
    select(id, mainstem, lengthkm, tot_drainage_areasqkm, order, hydroseq, hl_id, wb_id, areasqkm)
  
  nl$network = left_join(seed_network, fl_net, by = "id")
  
  nl$network_lookup = nl$network %>% 
    select(id, divide_id, wb_id, mainstem) %>% 
    mutate(hf_source = "NHDPlusV2", hf_id = id, hf_id_part = 1) %>% 
    select(id, hf_source, hf_id, hf_id_part, mainstem, divide_id, wb_id)

  nl$hydrolocations = read_sf(gpkg, poi_name) %>% 
    select(hl_id = identifier, id = COMID) %>% 
    rename_geometry("geometry")
  
  hl  = read_sf(gpkg, poi_name) %>% 
    mutate(hl_id = as.integer(identifier), id  = COMID,
           identifier = NULL, COMID = NULL, snapped = NULL, nexus = NULL)
  
  nl$hydrolocations_lookup = st_drop_geometry(hl) %>% 
    mutate_at(vars(matches("Type_")), as.character) %>%
    pivot_longer(-c(hl_id, id)) %>%
    filter(!is.na(value)) %>%
    mutate(hl_reference = gsub("Type_", "", name)) %>% 
    select(id, hl_id, hl_reference, hl_link = value) %>% 
    distinct() %>% 
    left_join(select(hl, hl_id), by = "hl_id") %>% 
    mutate(hl_position = "outflow")
  
  write_hydrofabric(nl, "data/test.gpkg", TRUE, enforce_dm = TRUE)
  
}

#' Convert Refactor Output to HF gpkg
#' This is a temporary function as changes get pushed upstream
#' @param gpkg gpkg file path
#' @return file.path
#' @export

#gpkg = get_hydrofabric(VPU = "01", 
# type = "refactor",
# dir = glue("{base}refactor"),
# overwrite = overwrite)

make_hf_gpkg_from_refactor = function(gpkg){
  
  if (is.null(gpkg)) {
    stop("hydrofabrics must be provided.")
  }
  
  nl = list()
  lyrs = st_layers(gpkg)$name
  
  flowpath_name = grep("flow", lyrs, value = TRUE)
  divides_name = grep("divides", lyrs, value = TRUE)
  poi_name = grep("POI", lyrs, value = TRUE)
  
  divides        = read_sf(gpkg, divides_name) %>% 
    select(divide_id = ID, areasqkm)
  
  sum(duplicated(nl$flowpaths$id))
  
  hydrolocations = read_sf(gpkg, poi_name) %>% 
    rename(id = ID, hl_id = identifier) %>% 
    select(id, hl_id, starts_with("Type_")) %>% 
    filter(!is.na(hl_id))
  
  read_sf(gpkg, 'mapped_POIs') %>% 
    filter(is.na(identifier))
  
  

  nl$flowpaths = read_sf(gpkg, flowpath_name) %>% 
    select(id = ID, toid = toID, mainstem = LevelPathID,
           lengthkm = LENGTHKM, tot_drainage_areasqkm = TotDASqKM, member_COMID) %>% 
    mutate(has_divide = id %in% divides$divide_id,
           network_type = ifelse(has_divide, "network", "connector"),
           divide_id = ifelse(has_divide, id, NA)) %>% 
    left_join(select(st_drop_geometry(divides), id = divide_id, areasqkm), by = "id")
  
  nl$flowpaths$hydroseq = get_sorted(select(st_drop_geometry(nl$flowpaths), id, toid))
  nl$flowpaths$order    = get_streamorder(select(st_drop_geometry(nl$flowpaths), ID = id, toID = toid))
  nl$flowpaths          = left_join(nl$flowpaths, select(st_drop_geometry(hydrolocations), id, hl_id), by = "id")
  
  nl$divides = divides %>% 
    mutate(has_flowline = divide_id %in% nl$flowpaths$id) %>% 
    mutate(id = ifelse(has_flowline, divide_id, NA),
           network_type = "network") %>% 
    left_join(select(st_drop_geometry(nl$flowpaths), id, toid), by = "id")
  
  nl$hydrolocations  = select(hydrolocations, id, hl_id) 
  
  nl$hydrolocations_lookup = st_drop_geometry(hydrolocations) %>% 
    mutate_at(vars(matches("Type_")), as.character) %>%
    pivot_longer(-c(hl_id, id)) %>%
    filter(!is.na(value)) %>%
    mutate(hl_reference = gsub("Type_", "", name)) %>% 
    select(id, hl_id, hl_reference, hl_link = value) %>% 
    distinct() %>% 
    left_join(select(hl, hl_id), by = "hl_id") %>% 
    mutate(hl_position = "outflow")

  seed_network = rbind(select(st_drop_geometry(nl$flowpaths),  divide_id, id, toid, network_type),
                       select(st_drop_geometry(nl$divides),    divide_id, id, toid, network_type)) %>% 
    filter(!duplicated(.)) %>% 
    mutate(has_flowline  = ifelse(network_type %in% c("coastal", "internal"), FALSE, TRUE),
           has_divide  = ifelse(network_type %in% c("connector"), FALSE, TRUE))
  
  member_ids = grep("member", names(nl$flowpaths), value = TRUE)
  
  if(length(member_ids) > 0 ){
    nl$network_lookup  =  nl$flowpaths %>%
      st_drop_geometry() %>%
      select(
        id = id,
        member  = !!member_ids,
        divide_id     = id,
        hl_id        = hl_id,
        mainstem = mainstem,
        hydroseq = hydroseq) %>%
      mutate(member = strsplit(member, ","),
             hl_id = as.integer(hl_id)) %>%
      unnest(col = 'member') %>%
      mutate(hf_id_part = sapply( strsplit(member, "[.]"), FUN = function(x){ x[2] }),
             hf_id_part = ifelse(is.na(hf_id_part), 1L, as.integer(hf_id_part)),
             hf_id = sapply( strsplit(member, "[.]"), FUN = function(x){ as.numeric(x[1]) }),
             member = NULL,
             hf_source = "NHDPlusV2"
      ) %>% 
      select(id, hf_source, hf_id, hf_id_part, mainstem, divide_id, hl_id)
  } 
  
  
  #  network
  nl$network = seed_network %>% 
    mutate(divide_id = ifelse(network_type == "connector", NA, id)) %>% 
    left_join(select(st_drop_geometry(nl$flowpaths), id, hl_id, mainstem, 
                     lengthkm, areasqkm, 
                     tot_drainage_areasqkm), by = "id") 
  
  # Flowpaths
  nl$flowpaths = rename_geometry(nl$flowpaths, "geometry")
  
  nl$hydrolocations = rename_geometry(nl$hydrolocations, "geometry")
  
  # Divides
  nl$divides = rename_geometry(nl$divides, "geometry")
  
  names(nl)
  write_hydrofabric(nl, "data/test.gpkg", TRUE, enforce_dm = TRUE)

}

