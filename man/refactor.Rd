% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refactor_wrapper.R
\name{refactor}
\alias{refactor}
\title{Refactoring Wrapper}
\usage{
refactor(
  gpkg = NULL,
  flowpaths = NULL,
  catchments = NULL,
  pois = NULL,
  avoid = NULL,
  split_flines_meters = 10000,
  collapse_flines_meters = 1000,
  collapse_flines_main_meters = 1000,
  cores = 1,
  fac = NULL,
  fdr = NULL,
  purge_non_dendritic = TRUE,
  keep = NULL,
  outfile = NULL
)
}
\arguments{
\item{gpkg}{a starting GPKG}

\item{flowpaths}{Reference flowline features}

\item{catchments}{Reference catchment features}

\item{avoid}{integer vector of COMIDs to be excluded from collapse modifications.}

\item{split_flines_meters}{numeric the maximum length flowpath desired in the output.}

\item{collapse_flines_meters}{numeric the minimum length of inter-confluence flowpath desired in the output.}

\item{collapse_flines_main_meters}{numeric the minimum length of between-confluence flowpaths.}

\item{cores}{integer number of cores to use for parallel execution}

\item{fac}{path to flow accumulation grid. If NULL (default) then catchments are NOT reconciled.}

\item{fdr}{path to flow direction grid. If NULL (default) then catchments are NOT reconciled.}

\item{keep}{proportion of points to retain in geometry simplification (0-1; default 0.05). See ms_simplify. If NULL, then no simplification will be executed.}

\item{outfile}{path to geopackage to write refactored_flowlines, and if facfdr != NULL, refactored catchments.}

\item{events}{data.frame containing events}
}
\value{
data to the specified gpkg
}
\description{
A wrapper around refactor_nhdplus and reconcile_catchment_divides
}
