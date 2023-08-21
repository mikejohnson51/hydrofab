pacman::p_load(hydrofabric, arrow)
devtools::load_all()

vpus = vpu_boundaries$VPUID[1:21]

base = '/Volumes/MyBook/nextgen'

## TASK 1: build out uniform catchment distribution ----
process = data.frame(
  vpus = vpus,
  outfiles = glue("{base}/uniform/uniform_{vpus}.gpkg"),
  global = glue("{base}/global_uniform/uniform_{vpus}.gpkg")
)

dir.create(dirname(process$outfiles[1]), showWarnings = FALSE)
dir.create(dirname(process$global[1]),   showWarnings = FALSE)

cw = read.csv(glue('{base}/CrosswalkTable_NHDplus_HU12.csv')) %>%
  select(id = FEATUREID, huc12 = HUC_12)

for (i in 1:nrow(process)) {
  
  VPU = process$vpus[i]
  
 
  
  refactored_gpkg = get_hydrofabric(VPU = VPU,
                                    type = "refactor",
                                    dir = glue("{base}/refactored"))
  
  reference_gpkg = get_hydrofabric(VPU = VPU,
                                   type = "reference",
                                   dir = glue("{base}/reference"))
  
  hl = read_sf(glue('{base}/hydrolocations/hl_{VPU}.gpkg')) 
  
  gpkg = aggregate_to_distribution(
    gpkg                   = refactored_gpkg,
    vpu                    = VPU,
    divide                 = 'refactored_divides',
    outfile                = process$outfiles[i],
    hydrolocations         = hl,
    overwrite = TRUE
  )
  
  gpkg = add_nonnetwork_divides(gpkg,
                                huc12 = cw,
                                reference_gpkg = reference_gpkg)
  
}


## TASK 2: Assign Globally Unique Identifiers ----

unlink(process$global)

gs_file = 'https://code.usgs.gov/wma/nhgf/reference-hydrofabric/-/raw/04cd22f6b5f3f53d10c0b83b85a21d2387dfb6aa/workspace/cache/rpu_vpu_out.csv'

modifications = read.csv(gs_file) %>% 
  filter(VPUID != toVPUID) %>% 
  rename(from = COMID, to = toCOMID)

meta = assign_global_identifiers(gpkgs = process$outfiles, 
                                 outfiles = process$global,
                                 modifications = modifications)

## TASK 3: Assign Globally Unique Identifiers
## 
for(i in 1:nrow(process)){
  try(append_style(process$global[i], layer_names = c("flowpaths", "divides", "hydrolocations")), silent = TRUE)
}
