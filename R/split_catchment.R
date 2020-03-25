get_neighbor_df <- function(row, col, nrow_mat, ncol_mat) {
  # Starting east going clockwise
  neighbors <- matrix(c(row,     col + 1, 16,
                        row + 1, col + 1, 32,
                        row + 1, col,     64,
                        row + 1, col - 1, 128,
                        row,     col - 1, 1,
                        row - 1, col - 1, 2,
                        row - 1, col,     4,
                        row - 1, col + 1, 8), nrow = 8, byrow = TRUE)

  neighbors <- data.frame(neighbors)
  names(neighbors) <- c("row", "col", "flow_to_test")
  rownames(neighbors) <- c("east", "southeast", "south", "southwest", "west", "northwest", "north", "northeast")

  if (row == nrow_mat) neighbors <- neighbors[!rownames(neighbors) %in% c("southeast", "south", "southwest"), ]
  if (col == ncol_mat) neighbors <- neighbors[!rownames(neighbors) %in% c("northeast", "east", "southeast"), ]
  if (row == 0) neighbors <- neighbors[!rownames(neighbors) %in% c("northwest", "north", "northeast"), ]
  if (col == 0) neighbors <- neighbors[!rownames(neighbors) %in% c("southwest", "west", "northwest"), ]

  return(neighbors)
}

find_upstream <- function(row_col, fdr_matrix) {

  row <- row_col[1]
  col <- row_col[2]

  # Hacking around the stop conditions in these three if statements.
  if (is.na(row) | is.na(col)) {
    return()
  }

  if (row <= 0 | row > nrow(fdr_matrix) | col <= 0 | col > ncol(fdr_matrix)) {
    return()
  }

  if (is.na(fdr_matrix[row, col])) {
    return()
  }

  neighbors <- get_neighbor_df(row, col, nrow(fdr_matrix), ncol(fdr_matrix))

  neighbor_ind <- cbind(neighbors$row, neighbors$col)
  flow_to_val <- neighbors$flow_to_test

  return(neighbor_ind[which(fdr_matrix[neighbor_ind] == flow_to_val), , drop = FALSE])
}

collect_upstream <- function(row_col, fdr_matrix) {
  
  m_size <- nrow(fdr_matrix) * ncol(fdr_matrix)
  
  out_cells <- matrix(NA_integer_, nrow = m_size, ncol = 2)
  
  check_cell_counter <- 1
  out_cell_counter <- 2
  
  out_cells[check_cell_counter, ] <- row_col
  
  row_col <- out_cells[check_cell_counter, ]
  
  while(!is.na(row_col[1])) {
    
    us <- find_upstream(row_col, fdr_matrix)
    
    new_rows <- nrow(us)
    
    if(!is.null(new_rows) && nrow(us) > 0) {
      
      out_cells[out_cell_counter:(out_cell_counter + new_rows - 1), ] <- us
      
      out_cell_counter <- out_cell_counter + new_rows
      
    }
    
    check_cell_counter <- check_cell_counter + 1
    
    row_col <- out_cells[check_cell_counter, ]
    
    if(check_cell_counter > m_size) {
      warning("checked all but didn't return?")
      browser()
    }
    
  }
  
  return(out_cells[1:check_cell_counter, ])
}

#' @title Split Catchment Divides
#' @description A catchment-divide splitting algorithm that works with a D8
#' flow direction grid and the output of nhdplus_refactor. See Vignette
#' for examples.
#' @param catchment sf data.frame with one catchment divide
#' @param fline sf data.frame with two or more flowline segments in
#' upstream downstream order.
#' @param fdr raster a flow direction raster that fully covers the catchment
#' @param fac raster a flow accumulation raster that fuller covers the catchment
#' @return Split catchment divides as an sfc geometry.
#' @importFrom raster raster crs crop mask rowColFromCell cellFromXY rasterToPolygons as.matrix
#' @importFrom dplyr group_by ungroup filter select mutate lead n
#' @importFrom sf st_crs st_crs<- st_coordinates as_Spatial st_buffer st_combine
#' st_as_sf st_as_sfc st_geometry st_simplify st_snap
#' st_difference st_cast st_sf st_area
#' @export
#'
split_catchment_divide <- function(catchment, fline, fdr, fac) {

  check_proj(catchment, fline, fdr)

  outlets <- st_coordinates(fline) %>%
    data.frame() %>%
    group_by(L1) %>%
    filter(dplyr::row_number() == n()) %>%
    ungroup()

  sp_cat_buffer <- as_Spatial(st_buffer(catchment, 200))

  fdr <- raster::crop(fdr, sp_cat_buffer,
                      snap = "out")
  fdr <- raster::mask(fdr, sp_cat_buffer)
  fac <- raster::crop(fac, sp_cat_buffer,
                      snap = "out")
  fac <- raster::mask(fac, sp_cat_buffer)

  return_cats <- list()

  fdr_matrix <- raster::as.matrix(fdr)
  fac_matrix <- raster::as.matrix(fac)

  if (nrow(fdr_matrix) != nrow(fac_matrix) | ncol(fdr_matrix) != ncol(fac_matrix)) {
    stop("flow direction and flow accumulation must be the same size")
  }

  for (cat in seq_len(nrow(outlets) - 1)) {
    in_out <- sf::st_within(sf::st_sfc(sf::st_point(c(outlets$X[cat],
                                                      outlets$Y[cat])),
                                       crs = sf::st_crs(fline)),
                            catchment, prepared = FALSE)[[1]]
    if (length(in_out) > 0 && in_out == 1) {

      cell <- raster::cellFromXY(fdr, c(outlets$X[cat], outlets$Y[cat]))
      row_col <- raster::rowColFromCell(fdr, cell)

      neighbors <- get_neighbor_df(row_col[1], row_col[2],
                                   nrow(fac_matrix), ncol(fac_matrix))
      neighbor_fac <- fac_matrix[cbind(neighbors$row, neighbors$col)]

      # Some flowline outlets don't hit the right raster cell.
      # This grabs a neighbor that is more than twice the flow accumulation
      # to avoid just going 1 cell in the downstream direction.
      if (any(neighbor_fac > (fac_matrix[row_col[1], row_col[2]]) * 2)) {
        new_rc <- neighbors[which(neighbor_fac == max(neighbor_fac)), 1:2]
        row_col[1] <- new_rc$row
        row_col[2] <- new_rc$col
      }

      us_cells <- collect_upstream(row_col, fdr_matrix)

      out <- matrix(0, nrow = nrow(fdr_matrix), ncol = ncol(fdr_matrix))

      out[us_cells] <- 1

      out <- raster::raster(out, template = fdr)

      raster_function <- function(x) x == 1

      out <- st_as_sf(
        raster::rasterToPolygons(out,
                                 fun = raster_function,
                                 dissolve = TRUE))


      smaller_than_one_pixel <- units::set_units(800, "m^2")
      snap_distance <- units::set_units(100, "m")

      ds_catchment <- st_geometry(out) %>%
        st_simplify(dTolerance = 40) %>%
        st_snap(st_geometry(catchment), tolerance = snap_distance)

      retry_cat_fun <- function(catchment, ds_catchment, smaller_than_one_pixel) {
        st_difference(st_geometry(catchment), ds_catchment) %>%
          st_cast("POLYGON") %>%
          st_sf() %>%
          mutate(area = st_area(.)) %>%
          filter(area > smaller_than_one_pixel) %>%
          st_combine()
      }

      suppressWarnings(st_crs(catchment) <- st_crs(ds_catchment))
      
      ds_catchment <- tryCatch(retry_cat_fun(catchment,
                                             ds_catchment,
                                             smaller_than_one_pixel),
                               error = function(e)
                                 retry_cat_fun(lwgeom::st_make_valid(catchment),
                                               lwgeom::st_make_valid(ds_catchment),
                                               smaller_than_one_pixel))

      us_catchment <- tryCatch(st_difference(st_geometry(catchment), ds_catchment),
                               error = function(e)
                                 st_difference(st_geometry(lwgeom::st_make_valid(catchment)),
                                               lwgeom::st_make_valid(ds_catchment)))

      catchment <- ds_catchment

      return_cats <- c(return_cats, us_catchment)
    } else {
      browser()
    }
  }

  return(st_as_sfc(c(return_cats, st_geometry(catchment)), crs = st_crs(catchment)))
}

check_proj <- function(catchment, fline, fdr) {
  proj <- as.character(raster::crs(fdr))
  if (sf::st_crs(catchment)$proj4string != proj |
      sf::st_crs(fline)$proj4string != proj |
      sf::st_crs(fline)$proj4string != sf::st_crs(catchment)$proj4string) {
    stop("All inputs must have the same projection.")
  }
  return(invisible(1))
}
