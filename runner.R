library(glue)
devtools::load_all()

sbtools::authenticate_sb("jjohnson@lynker.com", "Mj7-franklin-109034")


vpus  <- c("01", "08", "10L", "15", "02", 
           "04", "05", 
           "06", "07", "09", "03S", "03W", "03N",
           "10U", "11", "12",  "13", "14",  "16", "17", "18")


base = '/Volumes/Transcend/ngen/CONUS-hydrofabric/'
overwrite = TRUE

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
    log = TRUE
  ) 
  
  gpkg = add_nonnetwork_divides(gpkg, reference_gpkg = reference_gpkg) 
  
  gpkg = generate_lookup_table(gpkg, refactored_gpkg)
  
  gpkg = generate_catchment_network(gpkg)
  
}n


f = list.files('/Volumes/Transcend/ngen/CONUS-hydrofabric/uniform', 
           full.names = TRUE,
           pattern = "gpkg$")


sbtools::authenticate_sb("jjohnson@lynker.com", "Mj7-franklin-109034")

for(i in 1:length(f)){
  sbtools::item_append_files(sb_id("uniform"), f[i])
  message(basename(f[i]))
}

  