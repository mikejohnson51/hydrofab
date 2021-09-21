# nolint start
# options("rgdal_show_exportToProj4_warnings"="none")
extdata <- system.file("extdata", package = "hyRefactor")

test_divides <- sf::read_sf(file.path(extdata, "gage_01013500.gpkg"))

