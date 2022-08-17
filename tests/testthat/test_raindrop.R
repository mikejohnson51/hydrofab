context("raindrop")
test_that("basic raindrop", {
  source(system.file("extdata", "walker_data.R", package = "hydrofab"))

  start_point <- sf::st_sfc(sf::st_point(c(-122.7, 38.126)), crs = 4326)
  distance <- 100

  line <- sf::st_transform(dplyr::filter(walker_flowline, COMID == 5329435),
                           sf::st_crs(walker_fdr))

  fdr <- mask(walker_fdr, vect(line), inverse = TRUE)
  
  xy <- hydrofab:::trace_downstream(start_point, fdr, distance)

  expect_equal(nrow(sf::st_coordinates(xy)), 19)
})