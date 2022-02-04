context("aggregate network")

test_that("example runs", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))

  fline <- dplyr::right_join(dplyr::select(walker_flowline, COMID),
                             suppressWarnings(nhdplusTools::prepare_nhdplus(walker_flowline, 0, 0, 0, FALSE)))

  fline <- dplyr::select(fline, ID = COMID, toID = toCOMID,
                         LevelPathID = LevelPathI, Hydroseq)

  outlets <- data.frame(ID = c(5329357, 5329317, 5329365, 5329303, 5329435, 5329817),
                        type = c("outlet", "outlet", "outlet", "terminal", "outlet", "outlet"),
                        stringsAsFactors = FALSE)

  aggregated <- aggregate_network(fline, outlets)

  expect_equal(names(aggregated), c("cat_sets", "fline_sets"))

  expect_equal(nrow(aggregated$fline_sets), 12)

  outlets$ID[1] <- 10

  expect_error(aggregate_network(fline, outlets), "Outlet IDs must all be in flowpaths.")

  outlets$ID[1] <- 5329357
  outlets$type[1] <- "terminal"

  expect_error(aggregate_network(fline, outlets), "Terminal paths must have an NA or 0 toID")
})

test_that("minimal network", {
  source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
  fline <- walker_flowline

  outlets <- data.frame(ID = c(5329357, 5329317, 5329365, 5329435, 5329817),
                        type = c("outlet", "outlet", "outlet", "outlet", "outlet"))

  #' Add toCOMID
  fline <- nhdplusTools::get_tocomid(fline, add = TRUE)

  fline <- dplyr::select(fline, ID = comid, toID = tocomid,
                         levelpathid = levelpathi, hydroseq = hydroseq,
                         areasqkm = areasqkm, lengthkm = lengthkm)

  min_net <- get_minimal_network(fline, outlets)
  
  expect_equal(nrow(min_net), 8)
  
  expect_s3_class(min_net, "sf")  
  
  min_net <- get_minimal_network(sf::st_drop_geometry(fline), outlets)
  
  expect_s3_class(min_net, c("tbl_df","tbl","data.frame"), exact = TRUE) 
  
  expect_true(all(outlets$ID %in% min_net$ID))
})

test_that("missing outlet", {
  outlets <- as.data.frame(list(ID = c(496338, 21125133, 21047474, 249354, 
                                       21124683, 21124865, 21046242, 255614), 
                                type = c("outlet", "outlet", "outlet", "outlet", 
                                         "outlet", "outlet", "outlet", "outlet")))
  
  net <- readRDS(list.files(pattern = "test_outlets.rds", full.names = TRUE, recursive = TRUE))
  
  outlets <- hyRefactor:::make_outlets_valid(outlets, net)
  
  expect_true(21047070 %in% outlets$ID)
})

