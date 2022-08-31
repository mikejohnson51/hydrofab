context("split_lines")

test_that("split lines works", {

  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring",
                                                 where = "package:lwgeom",
                                                 mode = "function")) {

  source(system.file("extdata", "walker_data.R", package = "hydrofab"))

  flines <- suppressWarnings(nhdplusTools::prepare_nhdplus(walker_flowline, 0, 0))
  flines <- collapse_flowlines(flines, 1, F, 1)
  flines <- reconcile_collapsed_flowlines(flines)

  flines <- sf::st_as_sf(dplyr::inner_join(dplyr::select(walker_flowline, COMID),
                                           flines,
                                by = c("COMID" = "member_COMID"))) %>%
    dplyr::select(-COMID) %>%
    dplyr::distinct(.keep_all = TRUE) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(toID = toID[1], LENGTHKM = LENGTHKM[1],
              TotDASqKM = TotDASqKM[1]) %>%
    sf::st_cast("MULTILINESTRING") %>%
    dplyr::ungroup() %>%
    sf::st_line_merge() %>%
    rename(COMID = ID)

  split <- hydrofab:::split_lines(sf::st_transform(dplyr::select(flines, COMID),
                                                       5070), 250)

  expect_true(nrow(split) == 573)
  
  expect_true(all(split$split_fID_event == 1))
  
  event <- sf::st_sf(id = 1, 
                     COMID = 5329357, 
                     REACHCODE = "18050005000080", 
                     REACH_meas = 62.3175842930095, 
                     offset = 26.3950459356741,
                     sf::st_sfc(sf::st_point(c(-122.818325, 38.175753)), 
                                crs = 4326))
  
  expect_error(hydrofab:::split_lines(sf::st_transform(walker_flowline, 5070), 
                                                 max_length = 250, events = event),
               "lines must be class LINESTRING")
  
  split <- hydrofab:::split_lines(sf::st_transform(suppressWarnings(st_cast(walker_flowline, 
                                                                              "LINESTRING")), 
                                                     5070), 
                                    max_length = 250, events = event)
  
  expect_equal(nrow(split), 576)
  
  split <- hydrofab:::split_lines(sf::st_transform(suppressWarnings(st_cast(walker_flowline, 
                                                                              "LINESTRING")), 
                                                     5070), 
                                    max_length = 1000000, events = event)
  
  expect_equal(nrow(split), 2)
  
  expect_equal(split$event_REACHCODE, c("18050005000080", NA))
  
  expect_equal(split$event_REACH_meas, c(62.31758, NA), tolerance = 0.001)
  
  expect_equal(split$split_fID, c("5329357", "5329357.1"))
  }

})

test_that("split lines works", {

  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring",
                                                 where = "package:lwgeom",
                                                 mode = "function")) {

  flines_in <- readRDS("data/guadalupe_network_geom.rds")
  
  
  flines =  dplyr::inner_join(dplyr::select(flines_in, COMID),
                    sf::st_set_geometry(flines_in, NULL) %>%
                      nhdplusTools::prepare_nhdplus(0, 0, warn = FALSE),
                    by = "COMID") %>%
    sf::st_as_sf() %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_transform(5070)

  flines <- suppressWarnings(
    dplyr::inner_join(dplyr::select(flines_in, COMID),
                      sf::st_set_geometry(flines_in, NULL) %>%
                        nhdplusTools::prepare_nhdplus(0, 0, warn = FALSE),
                      by = "COMID") %>%
    sf::st_as_sf() %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_transform(5070) %>%
    split_flowlines(2000, para = 2))

  expect_true(length(which(grepl("1623361", as.character(flines$COMID)))) == 10)

  }

})

test_that("split_line_event", {
  
  event <- data.frame(REACHCODE = "03030002000097",
                      REACH_meas = 71.12384)
  
  line <- readRDS("data/event_split_line.rds")
  
  test <- hydrofab:::split_by_event(line, event)

  expect_equal(test$start, c(0, 0.289), tolerance = 0.01)
  expect_equal(test$end, c(0.289, 1), tolerance = 0.01)
  
  event <- data.frame(REACHCODE = c("03030002000097", "03030002000097", "03030002000097"),
                      REACH_meas = c(10, 40, 75))
  
  test <- hydrofab:::split_by_event(line, event)
  
  expect_equal(test$start, c(0, 0.25, 0.6, 0.9), tolerance = 0.01)
  expect_equal(test$end, c(0.25, 0.6, 0.9, 1), tolerance = 0.01)
})

test_that("split_flowlines at scale", {
  
  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring",
                                                 where = "package:lwgeom",
                                                 mode = "function")) {
    
    source(system.file("extdata", "new_hope_data.R", package = "hydrofab"))
    
    new_hope_flowline <- right_join(select(new_hope_flowline, COMID, REACHCODE, FromMeas, ToMeas), 
                                    suppressWarnings(prepare_nhdplus(new_hope_flowline, 0, 0, 0, FALSE, warn = FALSE)), by = "COMID")
    
    split <- split_flowlines(suppressWarnings(st_cast(st_transform(new_hope_flowline, 5070), "LINESTRING")), 20000000)
    
    expect_true(is.character(split$COMID))
    
    split <- split_flowlines(suppressWarnings(st_cast(st_transform(new_hope_flowline, 5070), "LINESTRING")), 2000, 
                             new_hope_events)
    
    expect_true("event_identifier" %in% names(split))
    
    expect_equal(nrow(split), 824)
    
    split <- split_flowlines(suppressWarnings(st_cast(st_transform(new_hope_flowline, 5070), "LINESTRING")), 2000)
    
    expect_equal(nrow(split), 817)
    
    one_path <- filter(new_hope_flowline, COMID %in% c(new_hope_events[3, ]$COMID))$LevelPathI
    
    n <- filter(new_hope_flowline, LevelPathI %in% one_path)
    e <- new_hope_events[3, ]
    
    s <- split_flowlines(suppressWarnings(st_cast(st_transform(n, 5070), "LINESTRING")), 1000)
    
    expect_equal(s$COMID[1], "8893348")
    expect_equal(s$COMID[6], "8893330.3")
    expect_equal(s$toCOMID[s$COMID == "8893330.3"], "8893348") # 8893330
    expect_equal(s$toCOMID[s$COMID == "8893348"], "8893374.1")
    
    s <- split_flowlines(suppressWarnings(st_cast(st_transform(n, 5070), "LINESTRING")), 2000000, e)
    
    expect_equal(as.numeric(sf::st_length(s)), 
                 c(848.46, 2986.85, 1161.69, 666.85), 
                 tolerance = 0.1)
  }
})

test_that("rescale", {
  expect_equal(hydrofab:::rf(50, 50, 100), 0)
  expect_equal(hydrofab:::rf(50, 0, 50), 100)
  expect_equal(hydrofab:::rf(25, 0, 50), 50)
  expect_error(hydrofab:::rf(75, 0, 50), "m must be between f and t")
})

