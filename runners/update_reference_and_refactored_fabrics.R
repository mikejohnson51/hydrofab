pacman::p_load(hydrofabric, glue, arrow) 
devtools::load_all()


vpus  <- c("01", "08", "10L", 
           "15", "02", "04", 
           "05", "06", "07", 
           "09", "03S", "03W", 
           "03N", "10U", "11", 
           "12",  "13", "14",  
           "16", "17", "18")

base = '/Volumes/Transcend/ngen/CONUS-hydrofabric'
dir.create(glue("{base}/refactored"))
dir.create(glue("{base}/reference"))

for(i in 1:length(vpus)){
  get_hydrofabric(VPU = "01", type = "reference", dir =  '/Volumes/Transcend/ngen/CONUS-hydrofabric/01_usgs_ref', overwrite = TRUE)
  #get_hydrofabric(VPU = vpus[i], type = "refactor", dir =  glue("{base}/refactored"), overwrite = TRUE)
  message(vpus[i])
}




get_refer