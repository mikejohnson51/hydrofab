context("match flowpaths")

test_that("match flowpaths runs", {
  source(system.file("extdata/nhdplushr_data.R", package = "hyRefactor"))

  source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))

  rm(list = ls()[!ls() %in% c("new_hope_flowline", "hr_catchment", "hr_flowline")])

  hr_flowline$TerminalPa <- 15000500003272

  suppressWarnings(matched <- match_flowpaths(source_flowline = new_hope_flowline,
                                              target_catchment = hr_catchment,
                                              target_flowline = hr_flowline))

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

