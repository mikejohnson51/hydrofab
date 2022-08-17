August 15, 2022

Merging `hyRefactor` with `hyAggregate` to create `hydrofab`
=================================================================================
  - Change `union_linestrings_geos` to `union_linestrings` - remove `rgeos` dependency
  - Change `union_polygons_geos` to `union_polygons` - remove `rgeos` dependency
  - Change `aggregate_network` to `aggregate_network_to_outlets`
  - Change `aggregate_catchments` to `aggregate_to_outlets`
  - Change `add_length` to `add_lengthm` to avoid conflicts with `add_lengthkm`
  
  - remove onAttach message
  
  - add `get_hydrofabric`, `read_hydrofabric`, and `write_hydrofabric`
  - add `prepare_network`
  - add `refactor` that wraps refactoring workflow
  - add `aggregate_along_mainstem`
  - add `collapse_headwaters`
  - add `aggregate_to_distribution`
  - add `add_nonnetwork_divides`
  - add `poi_to_outlet`
  - add `add_mapped_pois`
  - add `generate_lookup_table`
  
  # TODO: add subsetting utilites.
  
  - use I/O methods in all existing `hyRefactor` code
