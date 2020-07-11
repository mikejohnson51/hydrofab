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
  
  if(any(neighbor_ind == 0)) {
    keep <- neighbor_ind[, 1] != 0 &  neighbor_ind[, 2] != 0
  
    neighbor_ind <- neighbor_ind[keep, ]
  
    flow_to_val <- neighbors$flow_to_test[keep]
    
  } else {
    
    flow_to_val <- neighbors$flow_to_test
    
  }
  

  return(neighbor_ind[which(fdr_matrix[neighbor_ind] == flow_to_val), , drop = FALSE])
}

collect_upstream <- function(row_col, fdr_matrix, fac_matrix = NULL, flowpath_mode = FALSE) {

  m_size <- nrow(fdr_matrix) * ncol(fdr_matrix)
  
  out_cells <- matrix(NA_integer_, nrow = m_size, ncol = 2)
  
  check_cell_counter <- 1
  out_cell_counter <- 2
  
  out_cells[check_cell_counter, ] <- row_col
  
  row_col <- out_cells[check_cell_counter, ]
  
  while(!is.na(row_col[1])) {
    
    us <- find_upstream(row_col, fdr_matrix)
    
    if(flowpath_mode) {
      us <- flowpath_filter(us, fac_matrix)
    }
    
    new_rows <- nrow(us)
    
    if(!is.null(new_rows) && nrow(us) > 0) {
      
      out_cells[out_cell_counter:(out_cell_counter + new_rows - 1), ] <- us
      
      out_cell_counter <- out_cell_counter + new_rows
      
    }
    
    check_cell_counter <- check_cell_counter + 1
    
    row_col <- out_cells[check_cell_counter, ]
    
  }
  
  return(out_cells[1:check_cell_counter, ])
}

flowpath_filter <- function(us, fac_matrix) {
  fac_vals <- apply(us, 1, function(x, mat) {
    mat[x[1], x[2]]
  }, mat = fac_matrix)
  
  if(any(fac_vals > 0)) {
    us <- us[which(fac_vals == max(fac_vals)), , drop = FALSE]
  } else {
    us <- us[1, ]
  }
  
  return(us)
}

trace_upstream <- function(start_point, cat, fdr, fac_matrix, fdr_matrix) {

  s_p <- st_coordinates(start_point)
  
  suppressWarnings(row_col <- get_row_col(fdr, c(s_p[1], s_p[2]), fac_matrix))
  
  us_flowpath <- collect_upstream(row_col, fdr_matrix, fac_matrix, flowpath_mode = TRUE)
  
  us_flowpath <- us_flowpath[!is.na(us_flowpath[, 1]), ]
  
  xy_nodes <- matrix(c(raster::xFromCol(fdr, col = us_flowpath[, 2]),
                       raster::yFromRow(fdr, row = us_flowpath[, 1])), 
                     ncol = 2)
  
  xy_nodes <- rbind(start_point[[1]], xy_nodes)
  
  st_sfc(st_linestring(xy_nodes[nrow(xy_nodes):1, ]), crs = st_crs(start_point))
}

#' @title Split Catchment Divides
#' @description A catchment-divide splitting algorithm that works with a D8
#' flow direction grid and the output of nhdplus_refactor. See Vignette
#' for examples.
#' @param catchment sf data.frame with one catchment divide
#' @param fline sf data.frame with one or more flowline segments in
#' upstream downstream order.
#' @param fdr raster a flow direction raster that fully covers the catchment
#' @param fac raster a flow accumulation raster that fuller covers the catchment
#' @param lr boolean should catchments be split along the left/right bank?
#' @return Split catchment divides as an sfc geometry.
#' @importFrom raster raster crs crop mask rowColFromCell cellFromXY rasterToPolygons as.matrix
#' @importFrom dplyr group_by ungroup filter select mutate lead n
#' @importFrom sf st_crs st_crs<- st_coordinates as_Spatial st_buffer st_combine
#' st_as_sf st_as_sfc st_sfc st_geometry st_simplify st_snap
#' st_difference st_cast st_sf st_area st_distance st_within st_point
#' st_make_valid st_segmentize st_nearest_feature st_linestring
#' @importFrom nhdplusTools get_node
#' @export
#'
split_catchment_divide <- function(catchment, fline, fdr, fac, lr = FALSE) {

  check_proj(catchment, fline, fdr)

  outlets <- st_coordinates(fline) %>%
    data.frame() %>%
    group_by(L1) %>%
    filter(dplyr::row_number() == n()) %>%
    ungroup()

  suppressWarnings(fdr_matrix <- prep_cat_fdr_fac(catchment, fdr, fac))
  fdr <- fdr_matrix$fdr
  fac <- fdr_matrix$fac
  fac_matrix <- fdr_matrix$fac_matrix
  fdr_matrix <- fdr_matrix$fdr_matrix
  
  return_cats <- list()

  if (nrow(fdr_matrix) != nrow(fac_matrix) | ncol(fdr_matrix) != ncol(fac_matrix)) {
    stop("flow direction and flow accumulation must be the same size")
  }

  for (cat in seq_len(nrow(outlets) - 1)) {
    in_out <- st_within(st_sfc(st_point(c(outlets$X[cat],
                                                      outlets$Y[cat])),
                                       crs = st_crs(fline)),
                            catchment, prepared = FALSE)[[1]]
    if (length(in_out) > 0 && in_out == 1) {
      suppressWarnings(row_col <- get_row_col(fdr, c(outlets$X[cat], outlets$Y[cat]), fac_matrix))
      
      us_cells <- collect_upstream(row_col, fdr_matrix)
      
      out <- matrix(0, nrow = nrow(fdr_matrix), ncol = ncol(fdr_matrix))

      out[us_cells] <- 1

      out <- suppressWarnings(raster::raster(out, template = fdr))

      raster_function <- function(x) x == 1

      suppressWarnings(out <- st_as_sf(
        raster::rasterToPolygons(out,
                                 fun = raster_function,
                                 dissolve = TRUE)))


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
                                 retry_cat_fun(st_make_valid(catchment),
                                               st_make_valid(ds_catchment),
                                               smaller_than_one_pixel))

      us_catchment <- tryCatch(st_difference(st_geometry(catchment), ds_catchment),
                               error = function(e)
                                 st_difference(st_geometry(st_make_valid(catchment)),
                                               st_make_valid(ds_catchment)))

      catchment <- ds_catchment

      return_cats <- c(return_cats, us_catchment)
    } else {
      browser()
    }
  }

  out <- st_as_sfc(c(return_cats, st_geometry(catchment)), crs = st_crs(catchment))
  
  if(lr) {
    return(split_lr(out, fline, fdr, fac_matrix, fdr_matrix))
  }
  
  return(out)
}

split_lr <- function(cat, fline, fdr, fac_matrix, fdr_matrix) {
  
  out <- lapply(c(1:nrow(fline)), function(x, cat, fline, fdr, fac_matrix, fdr_matrix) {
    line <- st_cast(st_geometry(fline[x, ]), "LINESTRING")
    
    cat <- st_sfc(st_segmentize(cat[[x]], dfMaxLength = 100), crs = st_crs(cat))
    
    un <- st_geometry(get_node(line, position = "start"))
    dn <- st_geometry(get_node(line, position = "end"))
    
    coords <- st_cast(cat, "POINT")
    
    snap_un <- coords[st_nearest_feature(un, coords)]
    snap_dn <- coords[st_nearest_feature(dn, coords)]
    
    if(as.numeric(st_distance(un, snap_un)) > 100) {
      extra_line <- trace_upstream(un, cat, fdr, fac_matrix, fdr_matrix)
      
      extra_line <- st_simplify(extra_line, dTolerance = 50)
      
      line[[1]] <- st_linestring(rbind(extra_line[[1]], line[[1]]))
      
      un <- st_geometry(nhdplusTools::get_node(line, position = "start"))
      snap_un <- coords[st_nearest_feature(un, coords)]
    }
    
    line[[1]][1, ] <- st_coordinates(snap_un)
    line[[1]][nrow(line[[1]]), ] <- st_coordinates(snap_dn)
    
    split_cat <- lwgeom::st_split(cat, line)
    
    split_1 <- st_sfc(st_geometry(split_cat)[[1]][[1]], crs = st_crs(split_cat))
    split_2 <- st_sfc(st_geometry(split_cat)[[1]][[2]], crs = st_crs(split_cat))
    st_combine(c(split_1, split_2))
  }, fline = fline, cat = cat, fdr = fdr, fac_matrix = fac_matrix, fdr_matrix = fdr_matrix)
  
  do.call(c, out)
}

get_row_col <- function(fdr, start, fac_matrix) {
  cell <- raster::cellFromXY(fdr, start)
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
  
  return(row_col)
}

prep_cat_fdr_fac <- function(cat, fdr, fac) {
  sp_cat_buffer <- as_Spatial(st_buffer(cat, 200))
  
  fdr <- raster::crop(fdr, sp_cat_buffer,
                      snap = "out")
  fdr <- raster::mask(fdr, sp_cat_buffer)
  fac <- raster::crop(fac, sp_cat_buffer,
                      snap = "out")
  fac <- raster::mask(fac, sp_cat_buffer)
  
  fdr_matrix <- raster::as.matrix(fdr)
  fac_matrix <- raster::as.matrix(fac)
  
  return(list(fdr_matrix = fdr_matrix, 
              fac_matrix = fac_matrix,
              fdr = fdr,
              fac = fac))
}

check_proj <- function(catchment, fline, fdr) {
  proj <- as.character(raster::crs(fdr))
  if (st_crs(catchment) != st_crs(proj) |
      st_crs(fline) != st_crs(proj) |
      st_crs(fline) != st_crs(catchment)) {
    stop("All inputs must have the same projection.")
  }
  return(invisible(1))
}
