pacman::p_load(hydrofabric, glue, arrow) 
devtools::load_all()

vpus  <- c("01", "08", "10L", "15", "02", 
           "04", "05", 
           "06", "07", "09", "03S", "03W", "03N",
           "10U", "11", "12",  "13", "14",  "16", "17", "18")


base = '/Volumes/Transcend/ngen/CONUS-hydrofabric/'
overwrite = TRUE
cache = FALSE

## TASK 1: build out uniform catchment distribution

for(i in 2:length(vpus)){
  
  VPU = vpus[i]
  message(VPU)
  
  refactored_gpkg = get_hydrofabric(VPU = VPU, 
                                    type = "refactor",
                                    dir = glue("{base}refactor"),
                                    overwrite = FALSE)
  
  reference_gpkg = get_hydrofabric(VPU = VPU, 
                                   type = "reference",
                                   dir = glue("{base}reference"),
                                   overwrite = FALSE)
  
  gpkg = aggregate_to_distribution(
    gpkg            = refactored_gpkg,
    outfile         = glue("{base}uniform/uniform_{VPU}.gpkg"),
    outlets         = poi_to_outlet(gpkg = refactored_gpkg, verbose = FALSE),
    overwrite = overwrite,
    log = TRUE, cache = cache) 
  
  gpkg = add_nonnetwork_divides(gpkg, reference_gpkg = reference_gpkg) 
  
  gpkg = generate_lookup_table(gpkg, refactored_gpkg)
  
  gpkg = generate_catchment_network(gpkg)
  
}


## TASK 2: Assign Globally Unique Identifiers

gpkgs = list.files('/Volumes/Transcend/ngen/CONUS-hydrofabric/uniform', full.name = TRUE, pattern = "gpkg$")

meta = assign_global_identifiers(gpkgs = gpkgs, overwrite = TRUE)

write_parquet(meta$lookup, file.path(dirname(gpkgs[1]), "lookp_table.parquet"))



## TASK 3: Upload to ScienceBase

for(i in 1:length(gpkgs)){
  sbtools::item_append_files(sb_id("uniform"), gpkgs[i])
  message(basename(gpkgs[i]))
}

