context("sw catchment split")
test_that("long thin sw catchment", {
  
fac <- terra::rast(list.files(pattern = "sw_fac.tif$", 
                              full.names = TRUE, 
                              recursive = TRUE))

fdr <- terra::rast(list.files(pattern = "sw_fdr.tif$", 
                              full.names = TRUE, 
                              recursive = TRUE))
  
proj <- terra::crs(fdr)

flowline <- sf::read_sf(list.files(pattern = "sw.gpkg$", 
                                   full.names = TRUE, 
                                   recursive = TRUE), "fline") %>%
  st_transform(proj)

catchment <- sf::read_sf(list.files(pattern = "sw.gpkg$", 
                                    full.names = TRUE, 
                                    recursive = TRUE), "catchment") %>%
  st_transform(proj)

split <- hyRefactor::split_catchment_divide(catchment, flowline, fdr, fac)

expect_true(length(split) == 2)
})