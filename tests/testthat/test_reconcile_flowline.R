context("reconcile_collapse_flowlines")

test_that("reconcile collapse flowlines works as expected", {

  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  flines <- suppressWarnings(nhdplusTools::prepare_nhdplus(walker_flowline, 0, 0))
  flines <- collapse_flowlines(flines, 1, F, 1)
  flines <- reconcile_collapsed_flowlines(flines)

  get_id <- function(mc) {
    ind <- match(mc, flines$member_COMID)
    flines$ID[ind]
  }

  expect_equal(flines$member_COMID[which(flines$ID == get_id(5329323))],
               c(5329323, 5329325, 5329327))

  expect_equal(flines$toID[which(flines$ID == get_id(5329323))],
               c(get_id(5329817), get_id(5329817), get_id(5329817)))

  expect_true(flines$toID[which(flines$ID == get_id(5329321))] == get_id(5329323))
  expect_true(flines$toID[which(flines$ID == get_id(5329347))] == get_id(5329323))

  outlet <- flines[which(flines$member_COMID == "5329303"), ]
  expect_true(outlet$LevelPathID == outlet$Hydroseq,
         "Levelpath and hydroseq of outlet should be the same.")

  mainstem_headwater <- flines[which(flines$member_COMID == "5329435"), ]
  expect_true(mainstem_headwater$Hydroseq > outlet$Hydroseq,
         "Hydroseq of headwater should be greater than outlet.")
  expect_true(mainstem_headwater$LevelPathID == outlet$LevelPathID,
         "Levelpath of outlet and headwater should be the same.")

  mainstem <- arrange(flines[flines$LevelPathID == outlet$LevelPathID, ], Hydroseq)
  expect_true(all(mainstem$toID[!is.na(mainstem$toID)] %in% mainstem$ID),
         "Expect the mainstem to be well connected.")

  expect_true(nrow(mainstem) == 18, "Mainstem has 18 COMIDs")
  expect_true(tail(mainstem$member_COMID, 1) == "5329435",
         "Expect this to be the headwater of the mainstem.")
  expect_true(head(mainstem$member_COMID, 1) == "5329303",
         "Expect this to be the outlet of the mainstem.")

  expect_true(length(unique(walker_flowline$LevelPathI)) == length(unique(flines$LevelPathID)),
         "Expect the same number of level paths in both input and output.")
})

test_that("collapse works on a double pass", {

  nhdplus_flines <- readRDS(list.files(pattern = "oswego_network.rds", recursive = TRUE))
  split_flines_meters <- 2000
  split_flines_cores <- 2
  collapse_flines_meters <- 500
  collapse_flines_main_meters <- 500

  if (suppressWarnings(require(lwgeom)) &
      exists("st_linesubstring",
             where = "package:lwgeom",
             mode = "function")) {

    flines <- suppressWarnings(
        dplyr::inner_join(dplyr::select(nhdplus_flines, COMID),
                          sf::st_set_geometry(nhdplus_flines, NULL) %>%
                            nhdplusTools::prepare_nhdplus(0, 0),
                          by = "COMID") %>%
        sf::st_as_sf() %>%
        sf::st_cast("LINESTRING") %>%
        sf::st_transform(5070) %>%
        split_flowlines(split_flines_meters, para = split_flines_cores))

    collapsed_flines <-
      collapse_flowlines(sf::st_set_geometry(flines, NULL),
                         (0.25 * collapse_flines_meters / 1000),
                         TRUE,
                         (0.25 * collapse_flines_main_meters / 1000))

    collapsed_flines <-
      collapse_flowlines(collapsed_flines,
                         (0.5 * collapse_flines_meters / 1000),
                         TRUE,
                         (0.5 * collapse_flines_main_meters / 1000),
                         warn = FALSE)

    collapsed_flines <-
      collapse_flowlines(collapsed_flines,
                         (collapse_flines_meters / 1000),
                         TRUE,
                         (collapse_flines_main_meters / 1000),
                         warn = FALSE)

    # Old Tests:
    expect_equal(collapsed_flines$joined_toCOMID[
      which(collapsed_flines$COMID == "21975773")], "21975819.1")
    expect_equal(collapsed_flines$joined_toCOMID[
      which(collapsed_flines$COMID == "21976313")], "21975819.1")

    expect_equal(collapsed_flines$joined_fromCOMID[
      which(collapsed_flines$COMID == "21976891")],
                 collapsed_flines$joined_fromCOMID[
                   which(collapsed_flines$COMID == "21974583")])

    collapsed <- reconcile_collapsed_flowlines(flines = collapsed_flines,
                                               geom = select(flines, COMID),
                                               id = "COMID")

    collapsed[["member_COMID"]] <-
      unlist(lapply(collapsed$member_COMID,
                    function(x) paste(x, collapse = ",")))

    get_id <- function(mc) {
      ind <- match(mc, collapsed$member_COMID)
      collapsed$ID[ind]
    }

    expect_true("event_identifier" %in% names(collapsed))
    
    expect_true(collapsed$toID[which(collapsed$ID ==
                                  get_id("21976315,21975773,21976313,21975819.1"))] ==
             get_id("21975819.2"))
    expect_true(collapsed$toID[which(collapsed$ID == get_id("21975771.2"))] ==
             get_id("21976315,21975773,21976313,21975819.1"))
    expect_true(collapsed$toID[which(collapsed$ID == get_id("21975817"))] == get_id("21976253.1"))
    expect_true(collapsed$toID[which(collapsed$ID == get_id("21975819.2"))] == get_id("21975817"))

    outlet <- collapsed[which(collapsed$member_COMID == "21972746.2"), ]
    expect_true(outlet$LevelPathID == outlet$Hydroseq,
           "Levelpath and hydroseq of outlet should be the same.")

    mainstem_headwater <- collapsed[which(collapsed$member_COMID == "21983615.1"), ]
    expect_true(mainstem_headwater$Hydroseq > outlet$Hydroseq,
           "Hydroseq of headwater should be greater than outlet.")
    expect_true(mainstem_headwater$LevelPathID == outlet$LevelPathID,
           "Levelpath of outlet and headwater should be the same.")

    mainstem <- arrange(collapsed[collapsed$LevelPathID == outlet$LevelPathID, ], Hydroseq)
    expect_true(all(mainstem$toID[!is.na(mainstem$toID)] %in% mainstem$ID),
           "Expect the mainstem to be well connected.")

    expect_true(nrow(mainstem) == 166, "Mainstem has 166 COMIDs")
    expect_true(tail(mainstem$member_COMID, 1) == "21983615.1",
           "Expect this to be the headwater of the mainstem.")
    expect_true(head(mainstem$member_COMID, 1) == "21972746.2",
           "Expect this to be the outlet of the mainstem.")

    expect_true(length(unique(flines$LevelPathI)) == length(unique(collapsed$LevelPathID)),
           "Expect the same number of level paths in both input and output.")
  }
})
