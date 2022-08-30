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
#' @importFrom sf st_is_empty st_drop_geometry
#' @importFrom dplyr filter mutate left_join select distinct case_when bind_rows slice_min
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
#' aggregated <- aggregate_network(fline, outlets)
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
                              post_mortem_file = NA) {

  flowpath <- validate_flowpath(flowpath, outlets, post_mortem_file)

  # makes sure outlets connect
  outlets <- make_outlets_valid(outlets, drop_geometry(flowpath),
                                da_thresh = da_thresh,
                                only_larger = only_larger) %>%
    sort_outlets(flowpath)

  fline_sets <- get_catchment_sets(flowpath, outlets)
  
  #set should be the _new_ ID, ID should be the _old_ ID
  cat_sets <- fline_sets[[2]]

  fline_sets <- fline_sets[[1]]
  
  # create long form ID to set member list
  # OK to assume since Hydroseq is required in validate flowpath
  sets <- unnest_flines(fline_sets, "set") %>% 
    left_join(select(drop_geometry(flowpath), 
                     toSet = .data$toID,  .data$ID, .data$Hydroseq, .data$LevelPathID), 
              by = c("set" = "ID")) 
  
  # Figure out what the ID of the downstream catchment is.
  next_id = sets %>% 
    group_by(.data$ID) %>% 
    #LEVERAGE PRE SORT! 
    slice_min(.data$Hydroseq) %>% 
    ungroup() %>% 
    select(.data$ID, .data$toSet, .data$set, .data$LevelPathID) %>% 
    left_join(select(sets, toID = .data$ID,  .data$set), by = c("toSet" = "set")) %>% 
    select(.data$ID, .data$toID)

  
  if(inherits(flowpath, "sf")) {
    fline_sets <- select(sets, .data$set, .data$ID, .data$LevelPathID) %>% 
      left_join(select(flowpath, .data$ID), by = c("set" = "ID")) %>%
      st_as_sf() %>%
      filter(!st_is_empty(.)) %>%
      union_linestrings(ID = "ID") %>%
      left_join(next_id, by = "ID") %>% 
      left_join(fline_sets, by = "ID")
  } else {
    fline_sets =  left_join(fline_sets, next_id, by = "ID")  %>% 
      left_join(distinct(select(sets, .data$ID, .data$LevelPathID)), by = "ID")
  }

  cat_sets   <- left_join(cat_sets, next_id, by = "ID")

  return(list(cat_sets = cat_sets, fline_sets = fline_sets))
}

validate_flowpath <- function(flowpath, outlets, post_mortem_file) {

  flowpath <- check_names(flowpath, "aggregate_network")

  if (any(!outlets$ID %in% flowpath$ID)) stop("Outlet IDs must all be in flowpaths.")

  flowpath$toID[is.na(flowpath$toID)] <- 0

  if (any(flowpath$toID[flowpath$ID %in% outlets[outlets$type == "terminal", ]$ID] != 0)) {
    if(!is.na(post_mortem_file)) save(list = ls(), file = post_mortem_file)
    stop("Terminal paths must have an NA or 0 toID")
  }

  flowpath$toID <- methods::as(flowpath$toID, class(flowpath$ID))

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

apply_fix_nexus <- function(bad_id, flowpath, da_thresh, only_larger) {
  bind_rows(
    lapply(bad_id,
           function(bad_tail_id,
                    flowpath,
                    da_thresh,
                    only_larger) {
             fix_nexus(flowpath, bad_tail_id,
                       da_thresh, only_larger)
           },
           flowpath = flowpath,
           da_thresh = da_thresh,
           only_larger = only_larger))
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

  outlets$ID <- methods::as(outlets$ID, class(flowpath$ID))

  # Finds levelpaths and their unique head and outlet
  lps <- get_lps(drop_geometry(flowpath))

  # Need to check if outlets are on levelpath tails and add
  # required additional outlets.
  outlets <- bind_rows(
    outlets,
    apply_fix_nexus(filter(get_outlets(outlets, lps),
                           .data$ID == .data$tail_ID)$ID,
                    flowpath, da_thresh, only_larger)
  )

  # deduplicate outlets.
  outlets <- distinct(outlets) %>%
    group_by(.data$ID) %>%
    filter(!(n() > 1 & # this removes outlets that duplicate terminals.
               # they can be added in the above outlets check.
               .data$type == "outlet")) %>%
    ungroup()

  otl <- get_outlets(outlets, lps)

  count_while <- 0

  while (!all(otl$tail_ID %in% otl$ID)) {

    bad_tail <- otl$tail_ID[which(!otl$tail_ID %in% otl$ID)]

    message(paste("Fixing", length(bad_tail), "missing outlets."))

    outlets <- dplyr::bind_rows(
      outlets,
      apply_fix_nexus(bad_tail,
                      flowpath, da_thresh, only_larger)
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

sort_outlets <- function(outlets, flowpath) {
  # set toID to negative the ID for terminals that are not 0.
  # This is important to make sure that all outlet nexues have a unique ID
  # as opposed to 0 or na. This is meangful when sorting the graph
  flowpath$toNexID <- ifelse(flowpath$toID == 0, -flowpath$ID, flowpath$toID)

  # this is a little awkward and nuanced -- read comments carefully.
  outlets <- outlets %>%
    # the nexID_stem is just the catchment identifier. Conceptually,
    # this is the identifier for the nearest upstream nexus from an outlet.
    # We have to use this identifier for an outlet along the flowpath
    # of a catchment. It can be the same as the catchment because we are
    # working with a dendritic assumption so each nexus has one and only
    # one downstream catchment. Otherwise, we would have to treat things a
    # bit differently.
    mutate(nexID_stem = .data$ID) %>%
    # the nexID_terminal is the  to nexus of the terminal flowpath.
    # We need to treat this differently because an outlet of type
    # "terminal" must be downstream of all other outlets. Without a
    # unique nexus ID like this, we can not be gaurunteed a sort
    # order that will work correctly.
    left_join(select(drop_geometry(flowpath), .data$ID,
                     nexID_terminal = .data$toNexID), by = "ID") %>%
    # Now we can set the nexID (which is used to sort the outlets) appropriately.
    mutate(
      nexID = case_when(
        type == "outlet" ~ .data$nexID_stem,
        type == "terminal" ~ .data$nexID_terminal
      )
    ) %>%
    select(.data$ID, .data$type, .data$nexID) %>%
    distinct()

  fp_sort <- nhdplusTools::get_sorted(drop_geometry(select(flowpath, .data$ID, .data$toNexID)))$ID

  fp_sort <- c(fp_sort, flowpath$toNexID[flowpath$toNexID < 0])

  fp_sort <- fp_sort[fp_sort %in% outlets$nexID]

  left_join(data.frame(nexID = as.numeric(fp_sort)), outlets, by = "nexID") %>%
    select(-.data$nexID)
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
  outlets <- left_join(outlets, select(flowpath, .data$ID, .data$id), by = "ID")
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

unnest_flines <- function(x, col = "set") {
  
  times <- lengths(x[[col]])
  base_names <- names(x)[!names(x) == col]
  
  out <- as.data.frame(cbind(sapply(base_names, function(n) rep(x[[n]], times = times))))
  
  names(out) <- base_names
  
  out[[col]] <- unlist(x[[col]])
  
  out
}

get_catchment_sets <- function(flowpath, outlets) {

  fline_sets <- data.frame(ID = outlets$ID,
                           set = I(rep(list(list()), nrow(outlets))))

  cat_sets <- data.frame(ID = outlets$ID,
                         set = I(rep(list(list()), nrow(outlets))))

  flowpath <- prep_flowpath(flowpath)

  outlets <- prep_outlets(outlets, flowpath)

  # We'll start at all these
  heads <- get_heads(flowpath)

  # We can stop once these conditions have been met.
  outlet_count <- nrow(outlets)
  headwater_count <- length(heads)

  message(paste("Running", headwater_count, "headwaters for",
                outlet_count, "outlets."))

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

    if(h_c %% 1000 == 0) message(paste(h_c, "of", headwater_count))

  }

  set_length <- lengths(fline_sets$set)

  head_outlets <- outlets[set_length == 0, ] %>%
    left_join(distinct(select(flowpath, LevelPathID, ID, Hydroseq)), by = "ID") %>%
    select(LevelPathID, set, head_out_Hydroseq = Hydroseq)

  head_paths <- filter(flowpath, LevelPathID %in% head_outlets$LevelPathID) %>%
    left_join(head_outlets, by = "LevelPathID") %>%
    filter(Hydroseq >= .data$head_out_Hydroseq) %>%
    select(ID, set) %>%
    group_by(set) %>%
    summarise(ID = list(ID))

  fline_sets$set[head_paths$set] <- head_paths$ID

  cat_sets$set <- lapply(cat_sets$set, function(x) unique(x))

  list(fline_sets, cat_sets)
}

#' Get Minimal Network
#' @description Given a set of outlets, will generate a minimal network by
#' calling \code{\link{aggregate_network}} and adding nhdplus attributes to the result.
#'
#' If geometry is included with the network, it will be merged and returned.
#'
#' @inheritParams aggregate_network
#' @param flowpath sf data.frame Flowpaths with ID, toID, LevelPathID, 
#' Hydroseq and LENGTHKM and AreaSqKM attributes.

#' @return a data.frame (potentially including an sfc list column) with
#' attributes generated by \code{\link[nhdplusTools]{add_plus_network_attributes}}
#' and a list column "set" containing members of each output flowpath.
#' @importFrom nhdplusTools add_plus_network_attributes
#' @export
#' @examples
#' source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#' fline <- walker_flowline
#'
#' outlets <- data.frame(ID = c(5329357, 5329317, 5329365, 5329435, 5329817),
#'                       type = c("outlet", "outlet", "outlet", "outlet", "outlet"))
#'
#' #' Add toCOMID
#' fline <- nhdplusTools::get_tocomid(fline, add = TRUE)
#'
#' # get attributes set
#' fline <- dplyr::select(fline, ID = comid, toID = tocomid,
#'                        LevelPathID = levelpathi, hydroseq = hydroseq,
#'                        AreaSqKM = areasqkm, LENGTHKM = lengthkm)
#'
#' min_net <- get_minimal_network(fline, outlets)
#'
#' plot(sf::st_geometry(fline), col = "blue")
#' plot(sf::st_geometry(min_net), lwd = 2, add = TRUE)
#' plot(sf::st_geometry(nhdplusTools::get_node(min_net)), add = TRUE)
#'
get_minimal_network <- function(flowpath, outlets) {
  
  flowpath <- check_names(flowpath, "get_minimal_network")

  flowpath_sort <- left_join(
    data.frame(ID = flowpath$ID),
    nhdplusTools::get_sorted(flowpath[, c("ID", "toID"), drop = TRUE],
                             split = TRUE), by = "ID")

  terminal_paths <- unique(flowpath_sort$terminalID[flowpath_sort$ID %in% outlets$ID])

  # Grab terminal paths that matter and combine with outlets.
  outlets <- rbind(outlets,
                   data.frame(ID = terminal_paths,
                              type = "terminal"))

  flowpath <- flowpath[flowpath_sort$terminalID %in% terminal_paths, ]

  minimal <- aggregate_network(
    flowpath, 
    outlets = dplyr::filter(outlets, .data$ID %in% flowpath$ID),
    da_thresh = NA, only_larger = TRUE)

  min_net <- unnest_flines(select(drop_geometry(minimal$fline_sets), -LevelPathID)) %>%
    left_join(select(flowpath, .data$ID, .data$LENGTHKM,
                     .data$AreaSqKM, .data$LevelPathID),
              by = c("set" = "ID")) %>%
    group_by(ID) %>%
    summarise(toID = .data$toID[1],
              lengthkm = sum(.data$LENGTHKM),
              areasqkm = sum(.data$AreaSqKM),
              outlet_levelpath = min(.data$LevelPathID)) %>%
    mutate(toID = ifelse(is.na(.data$toID), 0, .data$toID)) %>%
    rename(comid = .data$ID,
           tocomid = .data$toID,
           nameID = .data$outlet_levelpath)%>%
    add_plus_network_attributes() %>%
    rename(ID = .data$comid, toID = .data$tocomid,
           outlet_nhdpv2_levelpath = .data$nameID,
           arbolate_sum = .data$weight) %>%
    left_join(select(minimal$fline_sets, .data$ID, .data$set),
                       by = "ID")

  if(inherits(minimal$fline_sets, "sf")) {
    sf::st_sf(min_net)
  } else {
    min_net
  }
}
