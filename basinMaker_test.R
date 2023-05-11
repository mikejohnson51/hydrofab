pacman::p_load(sf, nhdplusTools, dplyr, glue)
devtools::load_all()

# Waterloo Tests ----------------------------------------------------------

base = '/Users/mjohnson/Downloads/drainage_region_0006_v2-1'

catchments = read_sf(glue('{base}/finalcat_info_v2-1.shp')) %>%
  clean_geometry(., ID = "SubId") %>% 
  select(divide_id = ID, areasqkm, id = SubId, toid = DowSubId, wb_id = HyLakeId)

flowpaths  = read_sf(glue('{base}/finalcat_info_riv_v2-1.shp'))  %>% 
  select(ID = SubId, toID = DowSubId, wb_id = HyLakeId) %>% 
  mutate(length = add_lengthkm(.), nameID = NA, member_comid = NA) %>% 
  select(ID, toID, length, nameID, everything()) %>% 
  mutate(toID = ifelse(toID == -1, 0, toID)) %>% 
  mutate(weight = calculate_arbolate_sum(.))

fps2 = get_levelpaths(flowpaths, status = TRUE)
fps2 = rename(fps2, hydroseq = topo_sort, levelpathid = levelpath)

fps3 = left_join(flowpaths, fps2, by = "ID")
names(fps3) = tolower(names(fps3))

cats = catchments
num = 1000
while(num > 0){

  tmp = filter(cats, !id %in% fps3$id) %>% 
    select(id, newid = toid)
  
  tmp2 = select(filter(cats, id %in% tmp$newid), id) %>% 
    mutate(newid = id)
  
  message(nrow(tmp))
  
  new  = bind_rows(tmp, tmp2) %>% 
    union_polygons(ID = "newid") %>% 
    select(id = newid)
  
  cats = filter(cats, !id %in% c(tmp$id, tmp2$id)) %>% 
    bind_rows(new) %>% 
    select(id) %>% 
    inner_join(st_drop_geometry(select(catchments, id, toid, wb_id)))
  
  num = nrow(filter(cats, !id %in% fps3$id))
  message(num)
}

tmp = filter(catchments, wb_id  != 0) %>% 
  mutate(wb_id = as.character(wb_id))

ms  = filter(fps3, id %in% tmp$id) %>% 
  st_cast("LINESTRING") 

ends = st_set_geometry(ms, get_node(ms, "end")$geometry) %>% 
  select(id, toid, hydroseq) %>% 
  mutate(type = "wb_out") 

starts = st_set_geometry(ms, get_node(ms, "start")$geometry) %>% 
  select(id, toid, hydroseq) %>% 
  mutate(type = "wb_in") 
  
 nodes = bind_rows(starts, ends)  %>% 
   mutate(X = st_coordinates(.)[,1],
          Y = st_coordinates(.)[,2]) %>% 
   mutate(hl_id = 1:n())
 
 tmap = st_intersects(nodes, fps3)
 
 ll = data.frame(
   id            = rep(nodes$id,   times = lengths(tmap)),
   type       = rep(nodes$type, times = lengths(tmap)),
   X             = rep(nodes$X, times = lengths(tmap)),
   Y             = rep(nodes$Y, times = lengths(tmap)),
   touches       = fps3$id[unlist(tmap)]
 ) %>% 
   filter(id != touches) %>% 
   st_as_sf(coords = c("X", "Y"), crs = 4326) %>% 
   left_join(st_drop_geometry(select(fps3, id, hl_id = wb_id))) %>% 
   mutate(hl_id = as.character(hl_id)) %>% 
   mutate(id = ifelse(type == "wb_in", touches, id),
          touches = NULL)
 
hydrolocations = read_sf(glue('{base}/obs_gauges_v2-1.shp'))  %>% 
  mutate(type = "gage") %>% 
  select(id = SubId, type,  hl_id = Obs_NM) %>% 
  bind_rows(ll) %>% 
  mutate(hl_link = hl_id,
         hl_id = 1:n(),
         hl_position = "outlet",
         hl_reference = "waterloo") 

# Build -------------------------------------------------------------------

aggregate_to_distribution(
  flowpath = fps3,
  divide   = cats, 
  hydrolocations = hydrolocations,
  outfile = "data/ca_test_ngen.gpkg",
  overwrite = TRUE,
  cache = FALSE
)

#write_sf(non_dend, "data/ca_test_ngen.gpkg", "non_dend")
write_sf(hydrolocations, "data/ca_test_ngen.gpkg", "hydrolocations")
