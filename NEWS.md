August 15, 2022

Merging hyRefactor with hyAggregate
=================================================================================
  - Change union_linestrings_geos to union_linestrings - remove rgeos dependency
  - Change union_polygons_geos to union_polygons - remove rgeos dependency
  - Change aggregate_catchments to aggregate_catchments_to_outlets
  - Change aggregate_catchments to aggregate_to_outlets
  - remove onAttach message
  - add new I/O methods for gpkg and sf objects
  - add I/O method to aggregate_network_to_outlets