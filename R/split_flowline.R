#' @title Split Flowlines
#' @description A wrapper for split_lines that works on nhdplus attributes
#' @param flines data.frame with COMID, toCOMID, LENGTHKM
#' and LINESTRING sf column in "meters" projection
#' @param max_length maximum segment length to return
#' @param para numeric how many threads to use in parallel computation
#' @return All the flowlines with some split apart.
#' @importFrom dplyr group_by ungroup filter select mutate lead n right_join
#' @seealso The \code{\link{refactor_nhdplus}} function implements a complete
#' workflow using `split_flowlines()`.
#' @export
#'
split_flowlines <- function(flines, max_length, para = 0) {
  check_names(flines, "split_flowlines")

  geom_col <- attr(flines, "sf_column")

  split <- split_lines(flines, max_length, id = "COMID", para = para)

  if (!is.null(split)) {

  split <- left_join(split, sf::st_set_geometry(flines, NULL), by = "COMID")

  split <- group_by(split, COMID)

  split$part <- unlist(lapply(strsplit(split$split_fID, "\\."),
                              function(x) x[2]))

  split <- mutate(split, part = (ifelse(is.na(part), 0, as.integer(part)) + 1))

  # Assume flowdir is with digitized for now -- need to check in prep code.
  split <- ungroup(mutate(split,
                          toCOMID = ifelse(part == max(part),
                                           as.character(toCOMID),
                                           paste(lead(COMID),
                                                 lead(part), sep = "."))))

  split <- mutate(split, COMID = paste(COMID, part, sep = "."),
                  LENGTHKM = sf::st_length(split[[geom_col]]) / 1000)

  split <- sf::st_as_sf(select(split, -part, -split_fID))

  attr(split$LENGTHKM, "units") <- NULL
  split[["LENGTHKM"]] <- as.numeric(split[["LENGTHKM"]])

  remove_comid <- unique(as.integer(split[["COMID"]]))

  not_split <- filter(flines, !(COMID %in% remove_comid))

  flines <- rbind(not_split, split)

  # Rows with COMID like this need to be updated
  redirect_tocomid <- flines$COMID[which(grepl("\\.1$", flines$COMID))]

  old_tocomid <- gsub("\\.1$", "", redirect_tocomid)

  mutate(flines,
         toCOMID = ifelse(toCOMID %in% old_tocomid,
                          paste0(toCOMID, ".1"),
                          toCOMID))

  } else {
    flines %>%
      mutate(COMID = as.character(COMID),
             toCOMID = as.character(toCOMID))
  }
}




#' @title split lines
#' @description Splits lines longer than a given threshold into the
#' minimum number of pieces to all be under the given threshold.
#' @param input_lines data.frame of class sf with LINESTRING sfc column.
#' @param max_length maximum segment length to return
#' @param id name of ID column in data.frame
#' @param para how many cores to use
#' @return only the split lines.
#' @importFrom dplyr group_by ungroup filter select mutate
#' @noRd
#'
split_lines <- function(input_lines, max_length, id = "ID", para = 0) {
  if (max_length < 50) warning(paste("short max length detected,",
                                     "do you have your units right?"))

  geom_column <- attr(input_lines, "sf_column")

  input_crs <- sf::st_crs(input_lines)

  input_lines[["geom_len"]] <- sf::st_length(input_lines[[geom_column]])

  attr(input_lines[["geom_len"]], "units") <- NULL
  input_lines[["geom_len"]] <- as.numeric(input_lines[["geom_len"]])

  too_long <- filter(select(input_lines, id, geom_column, geom_len),
                     geom_len >= max_length)

  if (nrow(too_long) != 0) {

  rm(input_lines) # just to control memory usage in case this is big.

  too_long <- mutate(too_long,
                     pieces = ceiling(geom_len / max_length),
                     fID = seq_len(nrow(too_long))) %>%
    select(-geom_len)

  split_points <-
    sf::st_set_geometry(too_long, NULL)[rep(seq_len(nrow(too_long)),
                                            too_long[["pieces"]]), ] %>%
    select(-pieces)

  split_points <- split_points %>%
    group_by(fID) %>%
    mutate(split_fID = ifelse(dplyr::row_number() == 1,
                              as.character(fID),
                              paste0(fID, ".", dplyr::row_number() - 1))) %>%
    mutate(piece = 1:n()) %>%
    mutate(start = (piece - 1) / n(),
           end = piece / n()) %>%
    ungroup()

  new_line <- function(i, f, t) {
    lwgeom::st_linesubstring(x = too_long[[geom_column]][i],
                             from = f,
                             to = t)[[1]]
  }

  if (para > 0) {

    cl <- parallel::makeCluster(rep("localhost", 2), type = "SOCK")

    split_lines <- snow::parApply(cl, split_points[c("fID", "start", "end")],
                                  1,
                                  function(x) new_line(i = x[["fID"]],
                                                       f = x[["start"]],
                                                       t = x[["end"]]))

    parallel::stopCluster(cl)
  } else {
    split_lines <- apply(split_points[c("fID", "start", "end")], 1,
                         function(x) new_line(i = x[["fID"]],
                                              f = x[["start"]],
                                              t = x[["end"]]))
  }

  rm(too_long)

  split_lines <- sf::st_sf(split_points[c(id, "split_fID")],
                           split_geometry = sf::st_sfc(split_lines,
                                                 crs = input_crs))

  names(split_lines)[which(names(split_lines) ==
                             "split_geometry")] <- geom_column
  attr(split_lines, "sf_column") <- geom_column
  } else {
   split_lines <- NULL
 }
  return(split_lines)
}
