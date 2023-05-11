pacman::p_load(hydrofabric, arrow) 
devtools::load_all()

vpus  <- c("01", "08", "10L", 
           "15", "02", "04", 
           "05", "06", "07", 
           "09", "03S", "03W", 
           "03N", "10U", "11", 
           "12",  "13", "14",  
           "16", "17", "18")

base = '/Volumes/Transcend/ngen/CONUS-hydrofabric'
overwrite = TRUE
cache = FALSE

## TASK 1: build out uniform catchment distribution

process = data.frame(vpus = vpus,  
                     outfiles = glue("{base}/03_uniform/uniform_{vpus}.gpkg"),
                     global = glue("{base}/04_global_uniform/uniform_{vpus}.gpkg")) 

dir.create(dirname(process$outfiles[1]), showWarnings = FALSE)
dir.create(dirname(process$global[1]), showWarnings = FALSE)

cw = readr::read_csv('/Users/mjohnson/Downloads/CrosswalkTable_NHDplus_HU12.csv') %>%
  select(id = FEATUREID, huc12 = HUC_12)

unlink(process$outfiles)

for(i in 17:nrow(process)){
  
  VPU = process$vpus[i]
  
  refactored_gpkg = get_hydrofabric(VPU = VPU, 
                                    type = "refactor",
                                    dir = glue("{base}/02_refactored"),
                                    overwrite = FALSE)
  
  reference_gpkg = get_hydrofabric(VPU = VPU, 
                                   type = "reference",
                                   dir = glue("{base}/01_reference"),
                                   overwrite = FALSE)
  
  hl = hl_to_outlet(gpkg = refactored_gpkg, verbose = FALSE) %>% 
    mutate(hl_position = "outflow")
  
  gpkg = aggregate_to_distribution(
    gpkg                   = refactored_gpkg,
    vpu = process$vpus[i],
    divide                 = 'refactored_divides',
    outfile                = process$outfiles[i],
    hydrolocations         = hl,
    overwrite = TRUE,
    log       = TRUE, 
    cache     = FALSE
  ) 
  
  gpkg = add_nonnetwork_divides(gpkg, 
                                huc12 = cw,
                                reference_gpkg = reference_gpkg) 
  
}

## TASK 2: Assign Globally Unique Identifiers

unlink(process$global)

gs_file = 'https://code.usgs.gov/wma/nhgf/reference-hydrofabric/-/raw/04cd22f6b5f3f53d10c0b83b85a21d2387dfb6aa/workspace/cache/rpu_vpu_out.csv'

modifications = read.csv(gs_file) %>% 
  filter(VPUID != toVPUID) %>% 
  rename(from = COMID, to = toCOMID)

meta = assign_global_identifiers(gpkgs = process$outfiles, 
                                 outfiles = process$global,
                                 modifications = topo_modifications)




## TASK 3: Upload to ScienceBase

# for(i in 1:length(gpkgs)){
#   sbtools::item_append_files(sb_id("uniform"), gpkgs[i])
#   message(basename(gpkgs[i]))
# }

