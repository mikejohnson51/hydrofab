context("refactor wrapper")

test_that("refactor_nhdplus works as expected with three pass mode", {

  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring",
                                                 where = "package:lwgeom",
                                                 mode = "function")) {

  nhdplus_flines <- sf::st_zm(readRDS("data/north_network.rds"))

  split_flines_meters <- 2000
  split_flines_cores <- 3
  collapse_flines_meters <- collapse_flines_main_meters <- 1000
  out_collapsed <- "nhdplus_collapsed.gpkg"
  out_reconciled <- "nhdplus_reconciled.gpkg"

  flines <- suppressWarnings(sf::st_set_geometry(nhdplus_flines, NULL) %>%
    nhdplusTools::prepare_nhdplus(0, 0) %>%
    dplyr::inner_join(dplyr::select(nhdplus_flines, COMID), by = "COMID") %>%
    sf::st_as_sf() %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_transform(5070) %>%
    split_flowlines(split_flines_meters, split_flines_cores))

  collapsed_flines <-
    collapse_flowlines(sf::st_set_geometry(flines, NULL),
                       (0.25 * collapse_flines_meters / 1000),
                       TRUE,
                       (0.25 * collapse_flines_main_meters / 1000))

  collapsed_flines <-
    suppressWarnings(
      collapse_flowlines(collapsed_flines,
                         (0.5 * collapse_flines_meters / 1000),
                         TRUE,
                         (0.5 * collapse_flines_main_meters / 1000)))

  collapsed_flines <-
    suppressWarnings(
      collapse_flowlines(collapsed_flines,
                         (collapse_flines_meters / 1000),
                         TRUE,
                         (collapse_flines_main_meters / 1000)))

  collapsed <- reconcile_collapsed_flowlines(collapsed_flines,
                                             dplyr::select(flines, COMID),
                                             id = "COMID")

  collapsed$member_COMID <-
    unlist(lapply(collapsed$member_COMID,
                  function(x) paste(x, collapse = ",")))

  expect(collapsed$toID[which(collapsed$member_COMID == "5876083,5876435")] ==
           collapsed$ID[which(collapsed$member_COMID == "5875557")])

  # Taken care of in clean up! All kinds of wierd around this one in this test.
  expect(collapsed_flines$joined_fromCOMID[
    collapsed_flines$COMID == 5876435] == 5876083)

  }
})

test_that("The refactor_nhdplus function runs as expected", {
  if (suppressWarnings(require(lwgeom)) & exists("st_linesubstring",
                                                 where = "package:lwgeom",
                                                 mode = "function")) {

  nhdplus_flowlines <- sf::st_zm(readRDS("data/north_network.rds"))

  sink(file = "temp.txt") # Captures sf output
    m <- suppressWarnings( # Known warnings -- don't want.
      capture_messages(refactor_nhdplus(nhdplus_flines = nhdplus_flowlines,
                   split_flines_meters = 2000,
                   split_flines_cores = 3,
                   collapse_flines_meters = 500,
                   collapse_flines_main_meters = 500,
                   out_collapsed = "temp.gpkg",
                   out_reconciled = "temp_rec.gpkg",
                   three_pass = TRUE, warn = TRUE)))
    sink()

  expect(file.exists("temp.gpkg"))
  expect(file.exists("temp_rec.gpkg"))
  unlink("temp.gpkg")
  unlink("temp_rec.gpkg")
  unlink("temp.txt") # could test the contents of this file.

  expect_equal(m,
               c("flowlines split complete, collapsing\n",
        "collapse complete, out collapse written to disk, reconciling\n"))

  refactor_nhdplus(nhdplus_flines = nhdplus_flowlines,
                   split_flines_meters = 2000,
                   split_flines_cores = 3,
                   collapse_flines_meters = 500,
                   collapse_flines_main_meters = 500,
                   out_collapsed = "temp.gpkg",
                   out_reconciled = "temp_rec.gpkg",
                   three_pass = FALSE,
                   warn = FALSE)

  expect(file.exists("temp.gpkg"))
  expect(file.exists("temp_rec.gpkg"))

  unlink("temp.gpkg")
  unlink("temp_rec.gpkg")

  }
})