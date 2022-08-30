context("Geometry Fixes")

test_that("Reconciled catchments can be fixed...", {
  
  source(system.file("extdata", "geometry_data.R", package = "hyRefactor"))
  
  expect_false(length(st_cast(st_geometry(test_divides), "POLYGON")) == nrow(test_divides))
  
  divides_new = clean_geometry(test_divides, "ID", keep = .9)

  expect_true(length(st_cast(st_geometry(divides_new), "POLYGON")) == nrow(test_divides))
  
})

test_that("Make sure 'out' passes when NULL...", {
  
  catchments = readRDS(list.files(pattern = "null_geom_catch.rds$", 
                                  full.names = TRUE, 
                                  recursive = TRUE))
  
  divides_new = clean_geometry(catchments, ID = "ID", keep = .9)
  
  expect_true(nrow(divides_new) == 6)
  
})

test_that("union_linestrings_geos characterization", {
  
  # data for this was pulled from a browser session with example data.
  l <- sf::read_sf(list.files(pattern = "union_line_test.gpkg", recursive = TRUE))
  
  f <- l %>%
    hyRefactor:::drop_geometry() %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()
  
  o <- union_linestrings_geos(l, "ID")
  
  expect_equal(f$ID, o$ID)
  
  expect_s3_class(o, "sf")
  
  expect_true(all(grepl("LINESTRING", unique(sf::st_geometry_type(o)))))
  
  o <- union_linestrings(l, "ID")
  
  expect_true(all(f$ID %in% o$ID))
  
  expect_s3_class(o, "sf")
  
  expect_true(all(grepl("LINESTRING", unique(sf::st_geometry_type(o)))))
  
})
