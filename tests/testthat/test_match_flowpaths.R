context("match flowpaths")

test_that("match flowpaths runs", {
  source(system.file("extdata/nhdplushr_data.R", package = "hyRefactor"))
  
  source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
  
  rm(list = ls()[!ls() %in% c("new_hope_flowline", "hr_catchment", "hr_flowline")])
  
  hr_flowline$TerminalPa <- 15000500003272
  
  hr_catchment <- nhdplusTools:::rename_nhdplus(hr_catchment)
  suppressWarnings(hw_pair <- sf::st_join(get_hw_points(new_hope_flowline),
                                          dplyr::select(hr_catchment, FEATUREID),
                                          join = sf::st_within) %>%
                     st_set_geometry(NULL))
  
  suppressWarnings(matched <- match_flowpaths(source_flowline = new_hope_flowline,
                                              target_flowline = hr_flowline, 
                                              hw_pair = hw_pair))
  
  expect_equal(sum(!is.na(matched$mr_LevelPathI)), 1021)
  
  lp <- min(matched$mr_LevelPathI, na.rm = TRUE)
  mr_lp <- filter(new_hope_flowline, LevelPathI <= lp)
  hr_lp <- filter(matched, mr_LevelPathI <= lp)
  
  # validated manually
  expect_equal(nrow(mr_lp), 75)
  expect_equal(nrow(hr_lp), 139)
})

# # Visualize match
# lps <- unique(matched$mr_LevelPathI)
# lps <- lps[!is.na(lps)]
# for(lp in lps) {
#   png(paste0("png/", lp, ".png"), width = 1024, height = 1024)
#   par(mar = c(0, 0, 0, 0))
#   mr_lp <- filter(new_hope_flowline, LevelPathI <= lp)
#   hr_lp <- filter(matched, mr_LevelPathI <= lp)
#   plot(hr_flowline$geom, col = "blue", lwd = 0.5)
#   plot(mr_lp$geom, col = "red", lwd = 3, add = TRUE)
#   plot(hr_lp$geom, col = "black", add = TRUE)
#   dev.off()
# }

test_that("match flowpaths runs with RF1 and V2", {
  hw_pairs <- structure(list(ID = c(7849L, 7851L, 7853L, 7856L, 7857L, 7858L, 
                                    7860L, 7862L, 7863L, 7864L), 
                             FEATUREID = c(2171225L, 2171903L, 2171013L, 
                                           2171859L, 2171883L, 2171917L, 
                                           2172119L, 2171999L, 2172059L, 
                                           2171563L)), 
                        row.names = c(NA, -10L), 
                        class = c("tbl_df", "tbl", "data.frame"))
  rf1 <- sf::read_sf(system.file("extdata/rf1_test.gpkg", package = "hyRefactor"), "rf1_test")
  nhdp <- sf::read_sf(system.file("extdata/rf1_test.gpkg", package = "hyRefactor"), "nhdp_flowline")
  
  rf1_matches <- match_flowpaths(nhdp, rf1, hw_pairs)
  
  expect_equal(unique(rf1_matches$mr_LevelPathI[rf1_matches$headwater_COMID == 	2171013]), 290008321)
})


# viz results
# mrlp <- unique(rf1_matches$mr_LevelPathI)
# 
# plot_fun <- function(step, rf1, nhdp, mrlp) {
#   par(mar = c(0,0,0,0))
#   plot(st_geometry(rf1), lwd = 0.5)
#   if(step > 1) {
#     if(step %% 2 == 0) {
#       lps <- mrlp[1:(step/2)]
#     } else {
#       lps <- mrlp[1:((step-1)/2)]
#     }
#     plot(st_geometry(filter(nhdp, LevelPathI %in% lps)), lwd = 2, add =TRUE)
#     if(step %% 2 == 0) lps <- lps[1:(length(lps) - 1)]
#     
#     if(step > 2) 
#       plot(st_geometry(filter(rf1, ID %in% filter(rf1_matches, mr_LevelPathI %in% lps)$member_ID)), col = "red", add = TRUE)
#   }
# }
# 
# gifski::save_gif(lapply(1:(length(mrlp) * 2), plot_fun, 
#                         rf1 = rf1, nhdp = nhdp, mrlp = mrlp))


