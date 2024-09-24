add_prefix = function(topo,
                      hf_prefix = "cat-",
                      nexus_prefix = "nex-",
                      terminal_nexus_prefix = "tnx-",
                      coastal_nexus_prefix  = "cnx-",
                      internal_nexus_prefix = "inx-"){
  
  
  t1 = filter(topo, topo_type == "network") %>%
    mutate(nex_pre = case_when(
      type == "terminal" ~ terminal_nexus_prefix,
      type == "coastal"  ~ coastal_nexus_prefix,
      type == "internal" ~ internal_nexus_prefix,
      TRUE ~ nexus_prefix
    )) %>%
    mutate(id = paste0(hf_prefix, id),
           toid = paste0(nex_pre, toid),
           nex_pre = NULL)
  
  t2 = filter(topo, topo_type == "nexus") %>%
    mutate(nex_pre = case_when(
      type == "terminal" ~ terminal_nexus_prefix,
      type == "coastal"  ~ coastal_nexus_prefix,
      type == "internal" ~ internal_nexus_prefix,
      TRUE ~ nexus_prefix
    )) %>%
    mutate(id    = paste0(nex_pre,id),
           toid = paste0(hf_prefix, toid),
           nex_pre = NULL)
  
  bind_rows(t1,t2)   %>%
    select(id, toid, type, contains("vpu"))
  
}



flush_prefix	 = function (input, col) {
  for (i in col) {
    input[[i]] = as.numeric(gsub(".*-", "", input[[i]]))
  }
  input
}

add_nonnetwork_nexus_location  = function(divides,
                                          coastal_nexus_prefix = "cnx-",
                                          internal_nexus_prefix = "inx-",
                                          waterbody_prefix = "wb-"){
  
  coastal_nex = suppressWarnings({ st_point_on_surface(filter(divides, type == "coastal") ) }) %>%
    flush_prefix(c('divide_id', 'toid')) %>%
    mutate(divide_id = paste0(coastal_nexus_prefix, divide_id),
           type = "coastal",
           toid = paste0(waterbody_prefix, 0)) %>%
    select(divide_id, toid, type) %>%
    rename_geometry("geometry")
  
  internal_nex = suppressWarnings({ st_point_on_surface(filter(divides, type == "internal") ) }) %>%
    flush_prefix(c('divide_id', 'toid')) %>%
    mutate(divide_id = paste0(internal_nexus_prefix, divide_id),
           type = "internal",
           toid = paste0(waterbody_prefix, 0)) %>%
    select(divide_id, toid, type) %>%
    rename_geometry("geometry")
  
  return(bind_rows(coastal_nex, internal_nex) %>%
           rename(id = divide_id))
  
}


#' Realign Topology to a nexus network
#' @param network_list          list containing flowpath and catchment `sf` objects
#' @param nexus_prefix          character prefix for nexus IDs
#' @param terminal_nexus_prefix character prefix for terminal nexus IDs
#' @param coastal_nexus_prefix  character prefix for coastal nexus IDs
#' @param internal_nexus_prefix character prefix for internal nexus IDs
#' @param catchment_prefix      character prefix for catchment IDs
#' @param waterbody_prefix      character prefix for catchment IDs
#' @return list
#' @export
#' @importFrom  dplyr select mutate left_join bind_rows group_by mutate ungroup filter distinct bind_rows slice_max rename case_when
#' @importFrom  nhdplusTools rename_geometry get_node
#' @importFrom  sf st_drop_geometry st_intersects st_as_sf
# 
# filter(network_list$flowpaths, id == 3548) %>% 
#   mutate(l = add_lengthkm(.)) |> 
#   mapview::mapview()
# 
# filter(topo, id == 3548)

realign_topology = function(network_list,
                            nexus_prefix = NULL,
                            terminal_nexus_prefix = NULL,
                            coastal_nexus_prefix = NULL,
                            internal_nexus_prefix = NULL,
                            catchment_prefix = NULL,
                            waterbody_prefix = NULL,
                            term_add = 1e9,
                            term_filter = NULL){
  
  
  net = select(network_list$flowpaths, id, toid) |> 
    st_drop_geometry() 
  
  if(is.null(term_filter)){
  
    if(is.null(term_add)){
      term_net = filter(net, toid == 0 )
    } else {
      term_net = filter(net, toid >= term_add) 
    }
    
    ee2 = filter(network_list$flowpaths, id %in% term_net$id)
    
    net = bind_cols(id = ee2$id, toid = ee2$toid, st_coordinates(get_node(ee2, "end"))) |> 
      group_by(X, Y) |> 
      mutate(toid = cur_group_id() + term_add) |> 
      ungroup() |> 
      bind_rows(filter(net, toid < term_add))
    
    network_list$flowpaths$toid  = net$toid[match(network_list$flowpaths$id, net$id)]
    network_list$catchments$toid = net$toid[match(network_list$catchments$id, net$id)]
  } else {
    term_add = term_filter
  }
  
  # Isolate the flow network
  iso = select(network_list$flowpaths, id, toid, hydroseq, poi_id) 
  
  # Cast flow network to end nodes, these are the outlets of the flowpaths
  ends = iso  %>%
    rename_geometry('geometry') %>%
    mutate(geometry = get_node(., "end")$geometry) %>%
    left_join(st_drop_geometry(select(iso, id)), by = "id", multiple = "all")
  
  # Get all start and end node geometries
  starts_ends = bind_rows(get_node(iso, "start"), get_node(iso, "end"))
 # write_sf(starts_ends, "test.gpkg")
  # Find the locations where the end points intersect with starting/ending points
  emap     = st_intersects(ends, starts_ends)

  # If more then one intersection occurs its a nexus,
  #  otherwise it is a junction
  ends$type = ifelse(lengths(emap) > 1, "nex", "jun")
  
  # Now, intersect the typed ends with the isolated flow network
  tmap = st_intersects(ends, iso)
  
  # Build a data.frame that stores the following:
  # 1. ID           - the ID of the end node
  # 2. type         - the type of the end node
  # 3. touches      - the flowlines the end node touches
  # 4. hs           - the hydrosequence of the end node
  # 5. touches_toID - the toID of the flowline touched by the endnode
  
  # The data.frame is grouped by the ID (1) and the total entries are tabulated.
  # The data.frame is then joined to the isolated flownetwork to append the topologic toID
  
  df = data.frame(
    id            = rep(ends$id, times = lengths(tmap)),
    type          = rep(ends$type, times = lengths(tmap)),
    hs            = rep(ends$hydroseq, times = lengths(tmap)),
    touches       = iso$id[unlist(tmap)],
    touches_toID  = iso$toid[unlist(tmap)]) %>%
    group_by(id) %>%
    mutate(n = dplyr::n()) %>%
    ungroup() %>%
    left_join(st_drop_geometry(select(iso, id, toid)), by = "id", multiple = "all")
  
  ### --- UPDATE DF --- ###
  
  # tmp_topo = df %>%
  #   select(id, toid) %>%
  #   distinct() %>%
  #   rename(new_toid = toid)
  #
  # df2 = left_join(df, tmp_topo, by = "id")
  #
  # filter(df2, id == 1527)
  # 
  ### --- TERMINALS --- ###
  # Terminals are those where an end point only touches itself
  # ID/toID topology persists
  
  terminals = filter(df, toid >= term_add) %>%
    select(id, toid  = toid) %>%
    mutate(type = "terminal")
  
  straight_flow = filter(df, n == 1 & id == touches) |> 
    select(id, toid) |> 
    mutate(type = 'nexus')
  
  ### --- NEXUS 1 --- ###
  # nexuses are those typed as nex where the id and touches ID are not equal
  # ID/toID topology persists
  
  nexus    = filter(df, type == "nex") %>%
    filter(id != touches) %>%
    select(id, toid) %>%
    distinct() %>%
    mutate(type = "nexus")
  
  # Terminals and easy nexuses make the first set of nexus locations
  non_junction = bind_rows(terminals, straight_flow, nexus)
  
  ### --- Junctions --- ###
  # When flowlines have junctions involved (e.g. more then one incoming flowline)
  # we need to ensure the most upstream is selected as the nexus AND
  # that the others are topologically pushed downstream
  
  # First, all junctions are found
  juns    = filter(df, type == "jun") %>%
    filter(id != touches) |> 
    filter(!id %in% non_junction$id)
  
  multi_ids = filter(juns, duplicated(id))$id
  
  juns = filter(juns, id %in% multi_ids) %>%
    filter(touches_toID == toid) %>%
    bind_rows(filter(juns, !id %in% multi_ids))
  
  # The "top" junctions are those that do not touch any IDs in tmp1
  # AND that have the largest hydrosequnece
  # Here we shift the toID to the flowline it touches.
  
  top_juns = juns %>%
    filter(!touches %in% non_junction$toid) %>%
    group_by(touches) %>%
    slice_max(hs) %>%
    ungroup() %>%
    select(id, toid  = touches) %>%
    distinct() %>%
    mutate(type = "nexus")
  
  inner_term = juns %>%
    filter(!id %in% top_juns$id) %>%
    filter(touches_toID > term_add) %>%
    left_join(
      select(df, id, ds_term = touches_toID),
      by = c('toid' = "id"),
      relationship = "many-to-many") %>%
    left_join(
      select(df, toid, hs),
      by = c('ds_term' = "toid"),
      relationship = "many-to-many") %>%
    distinct() %>%
    filter(touches_toID != ds_term) %>%
    group_by(id) %>%
    slice_min(hs.y) %>%
    select(id, toid = ds_term) %>%
    mutate(type = "junction") %>%
    ungroup()
  
  # The "inner" junctions are those that are not top junctions
  # Here we shift the toID to the toID of the flowline it touches
  
  inner_juns = juns %>%
    filter(!id %in% c(top_juns$id, inner_term$id)) %>%
    select(id, toid  = touches_toID) %>%
    distinct() %>%
    mutate(type = "junction")
  
  if(nrow(top_juns) + nrow(inner_juns) + nrow(inner_term) != nrow(juns)){
    stop("To many junctions produced!", call. = FALSE)
  }
  
  # The complete nexus topo network is the combination of the first set,
  # The top junctions and the inner junctions
  # Collectively, these define the fl --> nex network topology
  
  topo = bind_rows(non_junction, top_juns, inner_term, inner_juns)  %>%
    mutate(topo_type = "network") |> 
    select(id, toid, topo_type, type) |> 
    distinct()

  
  ## NEW!!! ##
  xx =  select(topo, toid, type) %>%
    group_by(toid) %>%
    mutate(type = ifelse(any(type == 'terminal'), "terminal", type)) %>%
    slice(1) %>%
    ungroup()

  topo = topo %>%
    mutate(type = NULL) %>%
    left_join(xx, by = "toid", multiple = "all") %>%
    filter(id != toid)

  # We'll use the fl --> nex topo to modify the input flow network toIDs
  # Additionally we will add the NextGen required prefixes.
  fl =  left_join(select(network_list$flowpaths, -toid), 
                  topo, 
                  by = "id", 
                  multiple = "all")  %>%
    rename_geometry('geometry') %>%
    mutate(id = paste0(waterbody_prefix, id),
           toid = paste0(ifelse(type == "terminal", terminal_nexus_prefix, nexus_prefix), toid),
           realized_catchment = gsub(waterbody_prefix, catchment_prefix, id)) |> 
    filter(!duplicated(id))

  
  
  divide =  left_join(select(network_list$catchments, -toid),
                      select(topo, id, toid, net_type = type), 
                      by = "id")  %>%
    rename_geometry('geometry') %>%
    #filter(id == 24873) |> 
    mutate(toid = ifelse(type %in% c("coastal", "internal"), divide_id, toid),
           net_type = ifelse(is.na(net_type), type, net_type),
           type = ifelse(net_type == "terminal", "terminal", type)) %>%
    mutate(nex_pre = case_when(
      type == "terminal" ~ terminal_nexus_prefix,
      type == "coastal"  ~ coastal_nexus_prefix,
      type == "internal" ~ internal_nexus_prefix,
      TRUE ~ nexus_prefix
    )) %>%
    mutate(divide_id = paste0(catchment_prefix, divide_id),
           id = paste0(waterbody_prefix, id),
           toid = paste0(nex_pre, toid),
           topo_type = NULL,
           nex_pre = NULL) %>%
    select(divide_id, id, ds_id, toid, type, areasqkm, has_flowline) |> 
    filter(!duplicated(divide_id))
  
  # The nexuses defined so far are those part of the dendritic network,
  # We also want to add those that are coastal or inland.
  # We need to make POINT locations for
  # all nexus and terminal, coastal and inland divides
  
  nex = filter(fl, type %in% c("nexus",  "terminal")) %>%
    group_by(toid) |> 
    #ADDED!!
    slice_max(order) %>%
    slice_max(hydroseq) %>%
    ungroup() %>%
    mutate(geometry = get_node(., "end")$geometry,
           id = toid) %>%
    select(id, toid, poi_id, type = type) %>%
    flush_prefix(col = c("id", "toid")) %>%
    distinct() %>%
    mutate(nex_pre = case_when(
      type == "terminal" ~ terminal_nexus_prefix,
      type == "coastal"  ~ coastal_nexus_prefix,
      type == "internal" ~ internal_nexus_prefix,
      TRUE ~ nexus_prefix
    )) %>%
    mutate(id = paste0(nex_pre, id),
           toid = paste0(waterbody_prefix, toid),
           nex_pre = NULL) %>%
    mutate(toid = ifelse(type == "terminal", paste0(waterbody_prefix, 0), toid)) %>%
    bind_rows(
      add_nonnetwork_nexus_location(
        divide,
        coastal_nexus_prefix = coastal_nexus_prefix,
        internal_nexus_prefix = internal_nexus_prefix,
        waterbody_prefix = waterbody_prefix
      ))
  
  # We then add the nex --> fl topo to the existing fl --> nex topo
  topo = suppressWarnings({
    nex %>%
      flush_prefix(col = c('id', 'toid')) %>%
      select(id, toid, type) %>%
      st_drop_geometry() %>%
      mutate(topo_type = "nexus") %>%
      bind_rows(topo) %>%
      distinct()
  })
  

  
  if(sum(!divide$toid %in% nex$id) != 0){
    stop('Divides flow to nexus locations that do not exist!\n',
         paste(st_drop_geometry(divide[which(!divide$toid %in% nex$id),"divide_id"]),"-->", st_drop_geometry(divide[which(!divide$toid %in% nex$id),"toid"]), "\n"), call. = FALSE)
  }
  
 filter(divide, divide_id == 'cat-212673')
 filter(divide, divide_id == 'cat-212675')
  
  if(sum(!fl$toid %in% nex$id) != 0 ){
    stop('Flowpaths flow to nexus locations that do not exist!\n',
         paste(st_drop_geometry(fl[which(!fl$toid %in% nex$id),"id"]),"-->", st_drop_geometry(fl[which(!fl$toid %in% nex$id),"toid"]), "\n"), call. = FALSE)
  }
  
  if(sum(duplicated(divide$divide_id)) != 0 ){
    stop('Divides have duplicated IDs!!')
  }
  
  if(sum(duplicated(fl$id)) != 0 ){
    stop('Flowlines have duplicated IDs!!')
  }
  
  n = list(flowpaths = select(fl, -type, -topo_type),
           divides   = divide,
           nexus     = nex,
           topo      = topo
  )
  
  #write_hydrofabric(n, "data/test_16.gpkg", enforce_dm = FALSE)
  
  return(n)
  
}


#' @title Apply Nexus Topology
#' @description This function enforces the nexus-->flowpath topology and adds nexus locations,
#' a catchment edge list, a flowpath edge list, and a lookup_table to the
#' network_list object.
#' @param network_list          list containing flowpath and catchment `sf` objects
#' @param nexus_prefix          character prefix for nexus IDs
#' @param terminal_nexus_prefix character prefix for terminal nexus IDs
#' @param coastal_nexus_prefix  character prefix for coastal nexus IDs
#' @param internal_nexus_prefix character prefix for internal nexus IDs
#' @param catchment_prefix      character prefix for catchment IDs
#' @param waterbody_prefix      character prefix for catchment IDs
#' @param enforce_dm            should the data model be validated prior to writing?
#' @param export_gpkg           file path to write new data. If NULL list object is returned
#' @return list or file path
#' @importFrom sf read_sf st_point_on_surface
#' @importFrom dplyr select mutate left_join everything distinct contains slice n
#' @export

apply_nexus_topology = function(gpkg,
                                vpu = NULL,
                                nexus_prefix = "nex-",
                                terminal_nexus_prefix = "tnx-",
                                coastal_nexus_prefix = "cnx-",
                                internal_nexus_prefix = "inx-",
                                catchment_prefix = "cat-",
                                waterbody_prefix = "wb-",
                                term_add = 1e9,
                                term_filter = NULL,
                                verbose = TRUE,
                                enforce_dm = FALSE,
                                export_gpkg = NULL){
  
  hyaggregate_log("INFO", "\n--- Applying HY_feature topology ---\n", verbose)
  
  network_list = read_hydrofabric(gpkg, verbose = verbose)
  
  ngen_flows   = realign_topology(network_list,
                                  nexus_prefix          = nexus_prefix,
                                  terminal_nexus_prefix = terminal_nexus_prefix,
                                  coastal_nexus_prefix  = coastal_nexus_prefix,
                                  internal_nexus_prefix = internal_nexus_prefix,
                                  catchment_prefix      = catchment_prefix,
                                  waterbody_prefix      = waterbody_prefix,
                                  term_add = term_add, 
                                  term_filter = term_filter)
  
  if(!is.null(term_filter)){
    term_add = term_filter
  }
  
  if(!is.null(vpu)) { ngen_flows$topo$vpu = as.character(vpu) }
  
  ngen_flows$flowpaths = ngen_flows$flowpaths %>%
    select(id, toid,
           mainstem, order, hydroseq,
           lengthkm, areasqkm, tot_drainage_areasqkm,
           has_divide, divide_id = realized_catchment,
           poi_id, 
           contains("vpu"))
  
  ngen_flows$divides = ngen_flows$divides %>%
    select(divide_id, toid, type, ds_id, areasqkm, contains("vpu")) %>%
    left_join(st_drop_geometry(select(ngen_flows$flowpaths, id, divide_id, lengthkm, tot_drainage_areasqkm )), by = "divide_id") %>%
    mutate(areasqkm = add_areasqkm(.),
           has_flowline = !is.na(id))
  
  
  if(layer_exists(gpkg, "pois")){
      
    x = read_sf(gpkg, "pois") |>
      select(poi_id) |> 
      mutate(poi_id = as.integer(poi_id)) 
    
    y = st_drop_geometry(ngen_flows$flowpaths) |> 
      select(poi_id, id, nex_id = toid) |> 
      mutate(poi_id = as.integer(poi_id)) 
    
    ngen_flows$pois =  left_join(x,y, by = "poi_id") 
  }
  
  
  network = add_prefix(ngen_flows$topo,
                       hf_prefix = waterbody_prefix,
                       nexus_prefix = nexus_prefix,
                       terminal_nexus_prefix = terminal_nexus_prefix,
                       coastal_nexus_prefix = coastal_nexus_prefix,
                       internal_nexus_prefix = internal_nexus_prefix) %>%
    full_join(st_drop_geometry(select(ngen_flows$flowpaths,
                                      has_divide,
                                      id, divide_id,
                                      lengthkm,
                                      tot_drainage_areasqkm,
                                      mainstem,
                                      has_divide,
                                      contains("vpu"))),
              by = "id",
              multiple = "all") %>%
    full_join(st_drop_geometry(select(ngen_flows$divides, divide_id, areasqkm, n2 = type)), by = "divide_id") %>%
    mutate(type = ifelse(is.na(type), n2, type), n2 = NULL)
  
  if(layer_exists(gpkg, "pois")){
    t = st_drop_geometry(ngen_flows$pois) |> 
      select(id, poi_id) |> 
      distinct()
      
    network = left_join(network, t, by = "id", relationship = "many-to-many")
  }
  
  if(layer_exists(gpkg, "network")){
    tmp = read_sf(gpkg, "network") %>%
      mutate(id = paste0(waterbody_prefix, id),
             divide_id = paste0(catchment_prefix, divide_id),
             #hl_uri = as.character(hl_uri),
             toid = NULL,
             t = rowSums(is.na(.))) %>%
      group_by(id) %>%
      slice_min(t) %>%
      ungroup()
    
    sinks = filter(network, type %in% c(c('coastal', "internal"))) %>%
      select(divide_id) %>%
      filter(complete.cases(.)) %>%
      left_join(select(st_drop_geometry(ngen_flows$divides), divide_id, toid), by = 'divide_id') %>%
      left_join(select(tmp, -id), by = 'divide_id') %>%
      mutate(vpu = as.character(vpu),
             poi_id = as.integer(poi_id))
      
    
    ngen_flows$network  = filter(network, !type %in% c(c('coastal', "internal"))) %>%
      left_join(select(tmp,id, hf_source, hf_id, hf_id_part, hydroseq), by = 'id', relationship = "many-to-many") %>%
      bind_rows(sinks) %>%
      select(id, toid, divide_id, ds_id, mainstem, poi_id, hydroseq, poi_id, #hl_uri,
             hf_source, hf_id, lengthkm, areasqkm, tot_drainage_areasqkm,
             type, vpu)
  }
  
  # if(layer_exists(gpkg, "hydrolocations")){
  #   ngen_flows$nexus = ngen_flows$nexus %>%
  #     left_join(select(st_drop_geometry(ngen_flows$hydrolocations), #hl_uri,
  #                      poi_id),
  #               by = "poi_id",
  #               relationship = "many-to-many") %>%
  #     mutate(type = type, type = NULL) %>%
  #     group_by(id) %>%
  #     #mutate(hl_uri = paste(hl_uri, collapse = ",")) %>%
  #     distinct() %>%
  #     ungroup() %>%
  #     mutate(type = case_when(
  #       !is.na(poi_id) ~ "poi",
  #       TRUE ~ "nexus"
  #     ))
  # }
  
  ngen_flows$topo = NULL
  
  ## Add Nels Check ##
  ## >>> ndf[ ~ndf['id'].isin( cdf['toid'] ) ]
  baddies = filter(ngen_flows$nexus, !id %in% ngen_flows$divides$toid) %>%
    filter(toid != "wb-0")
  
  write_sf(baddies, export_gpkg, "error")
  
  if(nrow(baddies) > 0){
    message("Foiled again ... ID/toIDs: \n\t",
            paste(paste0(baddies$id, "-->", baddies$toid), collapse = "\n\t"))
  }
  
  if(!is.null(export_gpkg)){
    write_hydrofabric(ngen_flows, export_gpkg, enforce_dm = enforce_dm)
    return(export_gpkg)
  } else {
    return(ngen_flows)
  }
}
