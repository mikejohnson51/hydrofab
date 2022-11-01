pacman::p_load(hydrofabric, glue, arrow) 
devtools::load_all()

vpus  <- c("01", "08", "10L", "15", "02", 
           "04", "05", 
           "06", "07", "09", "03S", "03W", "03N",
           "10U", "11", "12",  "13", "14",  "16", "17", "18")


base = '/Volumes/Transcend/ngen/CONUS-hydrofabric/'
outdir = glue('{base}pre-release')
dir.create(outdir)
overwrite = FALSE
cache = FALSE

## TASK 1: build out uniform catchment distribution

process = data.frame(vpus = vpus,  outfiles = glue("{outdir}/uniform/uniform_{vpus}.gpkg")) %>% 
  mutate(global = glue("{gsub('uniform', 'global_uniform', outfiles)}"))

dir.create(dirname(process$outfiles[1]), showWarnings = FALSE)
dir.create(dirname(process$global[1]), showWarnings = FALSE)
process = process[1,]

#unlink(process$outfiles)

for(i in 1:nrow(process)){
  
  VPU = process$vpus[i]
  
  refactored_gpkg = get_hydrofabric(VPU = VPU, 
                                    type = "refactor",
                                    dir = glue("{base}refactor"),
                                    overwrite = overwrite)
  
  reference_gpkg = get_hydrofabric(VPU = VPU, 
                                   type = "reference",
                                   dir = glue("{base}reference"),
                                   overwrite = overwrite)
  
  gpkg = aggregate_to_distribution(
    gpkg            = refactored_gpkg,
    outfile         = process$outfiles[i],
    outlets         = poi_to_outlet(gpkg = refactored_gpkg, verbose = FALSE),
    overwrite = TRUE,
    log = TRUE, 
    cache = FALSE
  ) 

  gpkg = add_nonnetwork_divides(gpkg, reference_gpkg = reference_gpkg) 

  gpkg = add_lookup_table(gpkg, refactored_gpkg)
  
  # Mon Oct 31 14:47:00 2022 ------------------------------
  # enforce_hydro_dm
  
  # Flowpaths
  tmp = list()
  
  tmp$flowpaths = network_list$flowpaths %>% 
    select(id, toid, mainstem = levelpathid, 
           lengthkm, areasqkm, tot_drainage_areasqkm, 
           order, hydroseq) %>% 
    mutate(divide_id = id)
  
  # Divides
  
  tmp$divides = network_list$divides %>% 
    select(id, toid, areasqkm) %>% 
    mutate(divide_type = "network")
  
  # POIs
  tmp$POIs = network_list$mapped_POIs %>% 
    select(id, geometry) %>% 
    st_as_sf()
  
  #  network
  tmp$network = network_list$flowpaths %>% 
    select(id, toid, poi_id, mainstem = levelpathid,
           lengthkm, areasqkm, tot_drainage_areasqkm) %>% 
    st_drop_geometry()
  
  gpkg = add_lookup_table(gpkg, refactored_gpkg)

}


## TASK 2: Assign Globally Unique Identifiers

unlink(process$global)

meta = assign_global_identifiers(gpkgs = process$outfiles, outfiles = process$global)

## TASK 3: Upload to ScienceBase

# for(i in 1:length(gpkgs)){
#   sbtools::item_append_files(sb_id("uniform"), gpkgs[i])
#   message(basename(gpkgs[i]))
# }



r = read_sf("/Users/mjohnson/Downloads/infastructure.gpkg", "open_roads")
table(r$ff)
