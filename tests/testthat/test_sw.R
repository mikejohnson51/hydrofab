context("sw catchment split")
test_that("long thin sw catchment", {
fac <- raster::raster("data/sw_fac.tif")
fdr <- raster::raster("data/sw_fdr.tif")
proj <- as.character(raster::crs(fdr))

flowline <- read_sf("data/sw.gpkg", "fline") %>%
  st_transform(proj)
catchment <- read_sf("data/sw.gpkg", "catchment") %>%
  st_transform(proj)

split <- hyRefactor::split_catchment_divide(catchment, flowline, fdr, fac)

expect_true(length(split) == 2)
})