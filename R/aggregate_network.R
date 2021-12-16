#' Aggregate Network
#' @description Aggregates a catchment network according to a set of outlet.
#'
#' @param flowpath sf data.frame Flowpaths with ID, toID, LevelPathID, and Hydroseq attributes.
#'
#' @param outlets data.frame with "ID" and "type" columns. "ID" must be identifiers from
#' fowpath and divide data.frames. "type" should be "outlet", or "terminal".
#' "outlet" will include the specified ID.
#' "terminal" will be treated as a terminal node with nothing downstream.
#'
#' @param da_thresh numeric Defaults to NA. A threshold total drainage area in the
#' units of the TotDASqKM
#' field of the flowpath data.frame. When automatically adding confluences to make
#' the network valid, tributary catchments under this threshold will be lumped with
#' the larger tributaries rather than being added to the set of output catchments.
#'
#' @param only_larger boolean Defaults to TRUE. If TRUE when adding confluences to
#' make the network valid, only tributaries larger than the one with an upstream
#' outlet will be added. e.g. if a tributary is required in the model this will
#' add main stems that the tributary contributes to. Note that the NHDPlus treats
#' divergences as part of the main stem, so the da_thresh may still be needed to
#' eliminate small tributary catchments introduced by divergences near confluences.
#'
#' @param post_mortem_file rda file to dump environment to in case of error
#' 
#' @param mainstem_only logical only calculate mainstem network?
#'
#' @details This function operates on the catchment network as a node-edge graph.
#' The outlet types are required to ensure that graph searches start from the
#' appropriate nodes and includes the appropriate catchments. Outlets such as gages
#' should be treated as "outlet" outlets.
#' While it may be possible for the algorithm to determine terminal outlets, at this
#' time, it is required that they be specified explicitely as "terminal" outlet types.
#'
#' The function checks supplied outlets to make sure they connect downstream. Checks
#' verify that the outlet of the levelpath (main stem of a total catchment) of each
#' supplied outlet is
#' in the supplied outlet set. If the outlet of a levelpath is not in the supplied set, it
#' is added along with other catchments that contribute to the same receiving catchment.
#' These checks ensure that all output catchments have one and only one input and output
#' nexus and that all catchments are well-connected.
#'
#' @export
#' @importFrom igraph graph_from_data_frame topo_sort incident_edges V bfs head_of shortest_paths
#' @importFrom sf st_is_empty st_drop_geometry
#' @importFrom dplyr filter mutate left_join select distinct case_when bind_rows
#' @importFrom tidyr unnest_longer
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#'
#' fline <- dplyr::right_join(dplyr::select(walker_flowline, COMID),
#'                            nhdplusTools::prepare_nhdplus(walker_flowline, 0, 0, 0, FALSE))
#'
#' fline <- dplyr::select(fline, ID = COMID, toID = toCOMID,
#'                        LevelPathID = LevelPathI, Hydroseq)
#'
#' outlets <- data.frame(ID = c(5329357, 5329317, 5329365, 5329303, 5329435, 5329817),
#'                       type = c("outlet", "outlet", "outlet", "terminal", "outlet", "outlet"),
#'                       stringsAsFactors = FALSE)
#'
#' aggregated <- aggregate_network(fline, outlets)
#' 
#' aggregated <- aggregate_network(fline, outlets, mainstem_only = TRUE)
#' 
#' outlets <- dplyr::filter(fline, ID %in% outlets$ID)
#'
#' outlets <- nhdplusTools::get_node(outlets)
#'
#' plot(aggregated$fline_sets$geom, lwd = 3, col = "red")
#' plot(walker_flowline$geom, lwd = .7, col = "blue", add = TRUE)
#' plot(outlets$geometry, add = TRUE)
#' 
#'
aggregate_network <- function(flowpath, outlets,
                              da_thresh = NA, only_larger = FALSE,
                              post_mortem_file = NA, mainstem_only = FALSE) {

  ############
  # Reference Variables:
  # > flowpath: input flowpaths contain network topology
  # > outlets: input outlets that get augmented to form a connected network and
  #            get sorted in upstream downstream order.
  # > hycatchment: a data.frame containing catchment topology only using the same
  #                semantics as the names of the graph vertices and edges.
  # > cat_graph: an igraph object containing the hycatchment.
  # 
  # Mutated Variables:
  # > cat_sets: sets of aggregate catchments to be populated below
  # > fline_sets: sets of aggregate flowpaths to be populated below 
  # > include_verts: vertices that are still in scope.
  
  flowpath <- validate_flowpath(flowpath, outlets, post_mortem_file)

  # makes sure outlets connect
  outlets <- make_outlets_valid(outlets, drop_geometry(flowpath), 
                                da_thresh = da_thresh, 
                                only_larger = only_larger) %>%
    # cat_ID is to be used in a graph context
    mutate(catID = paste0("cat-", .data$ID)) %>%
    distinct()
  
  # Build hycatchment and nexus data.frames to preserve sanity in graph traversal.
  hycatchment <- drop_geometry(flowpath) %>%
    # set toID to negative the ID for terminals that are not 0.
    mutate(toID = ifelse(is.na(.data$toID), -.data$ID, .data$toID))

  # Join id to toID use ID as from nexus ID since we are assuming dendritic.
  # Not used but useful for debugging so commented
  # nexus <- left_join(select(hycatchment, toID = .data$ID),
  #                    select(hycatchment, fromID = .data$ID, .data$toID),
  #                    by = "toID") %>%
  #   mutate(nexID = paste0("nex-", .data$toID),
  #          fromID = paste0("cat-", .data$fromID),
  #          toID = paste0("cat-", .data$toID)) %>%
  #   select(.data$nexID, .data$fromID, .data$toID)

  # get fromID and toID straight with "nex-" prefix
  # This is modeled as "from" a nexus with the same numeric ID as the catchment 
  # which means that first-order catchments have a from node with the same id.
  # the toID is the numeric ID of the too catchment. The catchment id is the
  # numeric id of the currect catchment.
  hycatchment <- hycatchment %>%
    mutate(fromID = paste0("nex-", .data$ID),
           toID = paste0("nex-", .data$toID),
           catID = paste0("cat-", .data$ID)) %>%
    select(.data$fromID, .data$toID, .data$catID)
    
  # Convert the catchment network to a directed graph object
  cat_graph <- graph_from_data_frame(d = hycatchment,
                                     directed = TRUE)
  
  # Prepare outlets so they are just a type and a nexID
  outlets <- outlets %>%
    left_join(select(hycatchment, 
                     nexID_stem = .data$fromID, catID = .data$catID), by = "catID") %>%
    left_join(select(hycatchment, 
                     nexID_terminal = .data$toID, catID = .data$catID), by = "catID") %>%
    mutate(
      nexID = case_when(
        type == "outlet" ~ .data$nexID_stem,
        type == "terminal" ~ .data$nexID_terminal
      )
    ) %>%
    select(.data$catID, .data$ID, .data$type, .data$nexID) %>%
    distinct() %>%
    sort_outlets(cat_graph)

  ##### aggregate catchment identification #####

  fline_sets <- get_catchment_sets(flowpath, outlets)
  
  cat_sets <- fline_sets[[2]]
  
  fline_sets <- fline_sets[[1]]
  
  cat_sets <- select(cat_sets, -.data$nexID)
  
  cat_sets[["ID"]] <- as.numeric(gsub("^cat-", "", outlets$ID))
  
  fline_sets[["ID"]] <- as.numeric(gsub("^nex-", "", outlets$ID))
  
  # create long form ID to set member list
  sets <- tidyr::unnest_longer(drop_geometry(fline_sets), col = "set")
  
  # Figure out what the ID of the downstream catchment is.
  next_id <- left_join(sets, 
              select(drop_geometry(flowpath), .data$ID, .data$toID),
              by = c("set" = "ID")) %>%
    # first find the ID downstream of the outlet of each catchment.
    group_by(.data$ID) %>%
    filter(!.data$toID %in% .data$set) %>%
    select(.data$ID, .data$toID) %>%
    ungroup() %>%
    distinct() %>%
    # find the actual id of the catchment the one found above is a member of.
    left_join(select(sets, set_toID = .data$ID, .data$set),
              by = c("toID" = "set")) %>%
    select(.data$ID, toID = .data$set_toID)
  
  if(inherits(flowpath, "sf")) {
    fline_sets <- 
      data.frame(
        setID = unlist(fline_sets$set),
        ID = rep(fline_sets$ID, times = lengths(fline_sets$set))) %>% 
      left_join(select(flowpath, .data$ID), by = c("setID" = "ID")) %>% 
      st_as_sf() %>% 
      filter(!sf::st_is_empty(.)) %>% 
      union_linestrings_geos(ID = "ID") %>% 
      left_join(fline_sets, by = "ID")
  }
  
  fline_sets <- left_join(fline_sets, next_id, by = "ID")
  cat_sets   <- left_join(cat_sets, next_id, by = "ID")
  
  return(list(cat_sets = cat_sets, fline_sets = fline_sets))
}

validate_flowpath <- function(flowpath, outlets, post_mortem_file) {
  
  flowpath <- check_names(flowpath, "aggregate_network")
  
  if (any(!outlets$ID %in% flowpath$ID)) stop("Outlet IDs must all be in flowpaths.")
  
  flowpath$toID[flowpath$toID == 0] <- NA
  
  if (any(!is.na(
    flowpath$toID[flowpath$ID %in% outlets[outlets$type == "terminal", ]$ID]
  ))) {
    if(!is.na(post_mortem_file)) save(list = ls(), file = post_mortem_file)
    stop("Terminal paths must have an NA or 0 toID")
  }
  
  return(flowpath)
}

get_lps <- function(flowpath) {
  flowpath <- drop_geometry(flowpath) %>%
    select(.data$ID, .data$LevelPathID, .data$Hydroseq) %>%
    group_by(.data$LevelPathID)
  
  headwaters <- filter(flowpath, .data$Hydroseq == max(.data$Hydroseq)) %>%
    ungroup()
  
  outlets <- filter(flowpath, .data$Hydroseq == min(.data$Hydroseq)) %>%
    ungroup()
  
  left_join(ungroup(flowpath), select(headwaters, head_ID = .data$ID, .data$LevelPathID),
            by = "LevelPathID") %>%
    left_join(select(outlets, tail_ID = .data$ID, .data$LevelPathID),
              by = "LevelPathID")
}

# sorted version of the graph to re-order outlets in upstream-downstream order.
sort_outlets <- function(outlets, cat_graph) {
  cat_graph_sort_verts <- topo_sort(cat_graph)
  outlet_verts <- cat_graph_sort_verts[names(cat_graph_sort_verts) %in% outlets$nexID]
  outlets[match(names(outlet_verts), outlets$nexID), ]
}

# Get the levelpath outlet IDs for each of the input outlets.
get_outlets <- function(outlets, lps) {
  distinct(left_join(outlets,
                     select(lps, .data$ID, .data$LevelPathID, .data$tail_ID),
                     by = "ID"))
}

# Adds everything that contributes the same recieving catchment as a given tail id.
fix_nexus <- function(flowpath, tail_id, da_thresh = NA, only_larger = FALSE) {
  tail <- filter(flowpath, .data$ID == tail_id)
  
  add <- filter(flowpath, .data$toID == tail$toID)
  
  if (only_larger) {
    add <- filter(add, .data$LevelPathID <= tail$LevelPathID)
  }
  
  if (!is.na(da_thresh)) {
    add <- filter(add, .data$TotDASqKM > da_thresh)
  }
  # Can add functionality here to filter which Add IDs to include.
  add_ids <- add$ID
  
  data.frame(ID = add_ids,
             type = rep("outlet",
                        length(add_ids)),
             stringsAsFactors = FALSE)
}

fix_tail <- function(flowpath, outlets, toid_tail_id, da_thresh = NA, only_larger = FALSE) {
  potential_add <- fix_nexus(flowpath, toid_tail_id, da_thresh, only_larger)
  new <- !potential_add$ID %in% outlets$ID
  potential_add[new, ]
}

#' @description Given a set of outlets, works downstream adding outlets at the
#' outlet of each level path. This makes sure catchments get inserted so an upstream
#' catchment doesn't get orphaned in the middle of a larger catchment.
#' @param outlets the outlet list of gages, etc.
#' @param flowpath the reconciled flowline network
#' @importFrom dplyr filter distinct select left_join group_by mutate
#' @noRd
make_outlets_valid <- function(outlets, flowpath,
                               da_thresh = NA, only_larger = FALSE) {
  
  # Finds levelpaths and their unique head and outlet
  lps <- get_lps(drop_geometry(flowpath))
  
  outlets <- distinct(outlets) %>%
    group_by(.data$ID) %>%
    filter(!(n() > 1 & .data$type == "outlet")) %>%
    ungroup()
  
  otl <- get_outlets(outlets, lps)
  
  count_while <- 0
  
  while (!all(otl$tail_ID %in% otl$ID)) {
    
    bad_tail <- otl$tail_ID[which(!otl$tail_ID %in% otl$ID)]
    
    message(paste("Fixing", length(bad_tail), "missing outlets."))
    
    outlets <- dplyr::bind_rows(
      outlets,
      dplyr::bind_rows(lapply(
        bad_tail, function(bad_tail_id, flowpath, da_thresh, only_larger) {
          fix_nexus(flowpath, bad_tail_id, da_thresh, only_larger)
        }, flowpath = flowpath, da_thresh = da_thresh, only_larger = only_larger))
    )
    
    otl <- get_outlets(outlets, lps)
    
    count_while <- count_while + 1
    
    if (count_while > 20) {
      stop("Stuck in a while loop trying to fix disconnected outlets. Reduce drainage area threshold?")
    }
  }
  
  # Need to check that a "next down tributary" in the outlet set has a break along the
  # main stem that each outlet contributes to.
  otl <- left_join(otl, select(drop_geometry(flowpath),
                               .data$ID, .data$toID), by = "ID") %>%
    left_join(select(lps, .data$ID,
                     toID_hydroseq = .data$Hydroseq,
                     toID_tail_ID = .data$tail_ID,
                     toID_LevelpathID = .data$LevelPathID),
              by = c("toID" = "ID"))
  
  # this grabs the most downstream if duplicates were generated.
  otl <- group_by(otl, .data$ID) %>%
    filter(.data$toID_hydroseq == min(.data$toID_hydroseq)) %>%
    ungroup() %>%
    # This eliminates groups where only one thing goes to a given tail_ID.
    group_by(.data$toID_tail_ID) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # This grabs all the inflows to the nexus that each of these outlets is at.
  otl <- left_join(otl, select(drop_geometry(flowpath),
                               toID_fromID = .data$ID, .data$toID),
                   by = "toID") %>%
    mutate(type = "add_outlet")
  
  if (!is.na(da_thresh)) {
    otl <- left_join(otl, select(drop_geometry(flowpath),
                                 .data$ID, toID_fromID_TotDASqKM = .data$TotDASqKM),
                     by = c("toID_fromID" = "ID")) %>%
      filter(.data$toID_fromID_TotDASqKM > da_thresh)
  }
  
  if (only_larger) {
    otl <- left_join(otl, select(drop_geometry(flowpath),
                                 .data$ID, toID_fromID_lp = .data$LevelPathID),
                     by = c("toID_fromID" = "ID")) %>%
      filter(.data$toID_fromID_lp <= .data$toID_LevelpathID)
  }
  
  otl <- otl %>%
    select(ID = .data$toID_fromID, .data$type,
           LevelPathID = .data$toID_LevelpathID, .data$tail_ID) %>%
    distinct()
  
  # Need to verify that all ID == tail_ID instances are connected.
  # They might have been missed above.
  if (any(grepl("add_outlet", otl$type))) {
    otl$type <- "outlet"
    outlets <- distinct(rbind(outlets, otl[, c("ID", "type")]))
  }
  
  otl <- get_outlets(outlets, lps)
  tail_outlets <- which(otl$ID == otl$tail_ID & otl$type != "terminal")
  
  for (check in seq_along(tail_outlets)) {
    outlets <- rbind(outlets,
                     fix_tail(flowpath, outlets, otl$ID[check],
                              da_thresh = da_thresh, only_larger = only_larger))
    
    connected <- FALSE
    while (!connected) {
      toid <- filter(flowpath, .data$ID == otl[["ID"]][check])$toID
      toid_tail_id <- filter(lps, .data$ID == toid)[["tail_ID"]]
      
      if (!all(toid_tail_id %in% otl$tail_ID)) {
        outlets <- rbind(outlets,
                         fix_tail(flowpath, outlets, unique(toid_tail_id),
                                  da_thresh = da_thresh, only_larger = only_larger))
      } else {
        connected <- TRUE
      }
      otl <- get_outlets(outlets, lps)
    }
  }
  
  return(outlets)
}

prep_flowpath <- function(flowpath) {
  # flowpath <- drop_geometry(flowpath)
  
  # Make sure flowpath is sorted correctly
  flowpath <- arrange(flowpath, desc(Hydroseq))
  
  # do a little indirection with index ids
  flowpath$toid <- match(flowpath$toID, flowpath$ID)
  flowpath$id <- seq(1, nrow(flowpath))
  
  flowpath
}

prep_outlets <- function(outlets, flowpath) {
  outlets <- left_join(outlets, select(flowpath, ID, id), by = "ID")
  outlets$set <- 1:nrow(outlets)
  outlets
}

get_heads <- function(flowpath) {
  flowpath$id[!flowpath$id %in% flowpath$toid]
}

# need a little network walker function.
get_dwn <- function(ID, toid) {
  next_dn <- toid[ID]
  if(is.na(next_dn)) {
    return(ID)
  } else {
    return(c(ID, get_dwn(next_dn, toid)))
  }
}

my_combine <- function(old, new) {
  # can optimize later
  if(identical(old, list())) {
    new
  } else {
    c(old, new)
  }
}

get_catchment_sets <- function(flowpath, outlets) {
  
  fline_sets <- data.frame(ID = outlets$nexID,
                           set = I(rep(list(list()), nrow(outlets))))
  
  cat_sets <- data.frame(ID = outlets$catID,
                         nexID = outlets$nexID,
                         set = I(rep(list(list()), nrow(outlets))))
  
  flowpath <- prep_flowpath(flowpath)
  
  outlets <- prep_outlets(outlets, flowpath)
  
  # We'll start at all these
  heads <- get_heads(flowpath)
  
  # We can stop once these conditions have been met.
  outlet_count <- nrow(outlets)
  headwater_count <- length(heads)
  
  # Set up counters for the while loop
  o_c <- 1
  h_c <- 1
  
  while(h_c <= headwater_count) {
    head <- heads[h_c]
    
    path <- get_dwn(head, flowpath$toid)
    
    sets <- outlets$id %in% path
    
    breaks <- outlets[sets, ]
    nr_breaks <- nrow(breaks)
    
    # If this path only goes to one outlet, it is within an aggregate
    ## do nothing. Or maybe we can use this for aggregation later?
    # If this path goes through more than one outlet, it contains flowpaths.
    if(nr_breaks > 1) {
      paths <- split(path,
                     cut(path,
                         breaks = c(0, breaks$id),
                         labels = c(breaks$set)))

      cat_sets$set[as.integer(names(paths))] <- 
        lapply(names(paths), function(x) {
          my_combine(cat_sets$set[as.integer(x)][[1]], 
                     flowpath$ID[paths[x][[1]]])
        })
      
      path_outlets <- sapply(paths, function(x) x[length(x)])
      
      flowpath$toid[path_outlets] <- NA
      
      # the top one isn't useful for mainstems.
      paths <- paths[2:length(paths)]
      
      fline_sets$set[as.integer(names(paths))] <-
        lapply(paths, function(x) flowpath$ID[x])
      
      o_c <- o_c + (nr_breaks - 1)
      
    } else {
      
      outlet <- outlets[outlets$id == path[length(path)], ]
      
      # I think this can be added tothe flowpath finding for free
      cat_sets$set[outlet$set][[1]] <- my_combine(cat_sets$set[outlet$set][[1]], 
                                                  flowpath$ID[path])
      
    }
    
    h_c <- h_c + 1
    
  }
  
  set_length <- lengths(fline_sets$set)
  
  head_outlets <- outlets[set_length == 0, ] %>%
    left_join(distinct(select(flowpath, LevelPathID, ID, Hydroseq)), by = "ID") %>%
    select(LevelPathID, set, head_out_Hydroseq = Hydroseq)
  
  head_paths <- filter(flowpath, LevelPathID %in% head_outlets$LevelPathID) %>%
    left_join(head_outlets, by = "LevelPathID") %>%
    filter(Hydroseq >= head_out_Hydroseq) %>%
    select(ID, set) %>%
    group_by(set) %>%
    summarise(ID = list(ID))
  
  fline_sets$set[head_paths$set] <- head_paths$ID
  
  cat_sets$set <- lapply(cat_sets$set, function(x) unique(x))
  
  list(fline_sets, cat_sets)
}

