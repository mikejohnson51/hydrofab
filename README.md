# hyRefactor

[![Build Status](https://travis-ci.org/dblodgett-usgs/hyRefactor.svg?branch=master)](https://travis-ci.org/dblodgett-usgs/hyRefactor) [![Coverage Status](https://coveralls.io/repos/github/dblodgett-usgs/hyRefactor/badge.svg?branch=master)](https://coveralls.io/github/dblodgett-usgs/hyRefactor?branch=master)

## Tools for Manipulating the NHDPlus Network

This package is a growing collection of tools for manipulation of hydrographic
data built around the NHDPlus and HY_Features data model. There is no specific
funding or plan to continue development of this package long term.

### Installation:

```
install.packages("devtools")
install.packages("rgeos", repos="http://R-Forge.R-project.org", type="source")
devtools::install_github("dblodgett-usgs/hyRefactor")
```

### Terminology: 

The following definitions have been used as much as possible throughout the package.  
Terms for rivers:  
**Flowline:** The NHD name for a hydrographic representation of a flowing body of water. Flowline is generally used when referring to geometry.  
**Flowpath:** The HY_Features name for a hydrologic feature that is the primary path water follows through a catchment; either from headwater to outlet or inlet to outlet. Flowpath is used when describing aspects of the abstract flowpath featuretype, generally in relation to a flowpath's relationship to a catchment.  

Terms used for hydrologic units:  
**Catchment:** The most abstract unit of hydrology in HY_Features is the catchment. It is a physiographic unit with zero or one inlets and one outlet. It does not inherently have any conceptual realizations. Rather, a given catchment can be realized in a number of ways; flowpath, divide, and networks of flowpaths and divides are the primary realizations.  
**Catchment divide:** NHD "catchment" polygons are more accurately described as "catchment divide" features. Because of the overlap with the HY_Features abstract "catchment" feature type, "catchment divide" is used for polygon represenations of catchments.  

Terms used to describe network transormations:  
**Collapse:** Combining complex hydrology typified by very small inter-confluence catchments and splitting very large catchments.
**Reconcile:** Applying changes to the catchment network that result in a new well connected and valid version of the network.
**Aggregate:** Combining hydrologic units to create new, larger catchments that resolve to the outlets of a specified set of pre-existing catchments. In this case, "aggregate" is rooted in the HY_Features HY_CatchmentAggregate feature type.

### Functional Vision
The following describe a vision for the functionality that should be included
in the package in the long run.

##### Refactoring
The concept of refactoring as intended here includes:

1) aggregating catchments into groups based on existing network topology,  
2) collapsing catchment topology to eliminate small catchments,  
3) splitting large or long catchments to create a more uniform catchment size
distribution.

This type of functionality is especially relevant to modeling applications that
need specific modeling unit characteristics but wish to preserve the network as
much as possible for interoperability with other applications that use the
NHDPlus network.

### Data Model
Given that development of this package was is focused on working with NHDPlus data, 
the NHDPlus data model will largely govern the data model the package is designed to work
with. That said, much of the package functionality also uses concepts from
the HY\_Features standard.  

*Note:* The HY\_Features standard is based on the notion that a "catchment" is a
wholistic feature that can be "realized" (some might say modeled) in a number of
ways. In other words, a catchment can *only* be characterized fully through a
collection of different conceptual representations. In NHDPlus, the "catchment"
feature is the polygon feature that describes the drainage divide around the
hydrologic unit that contributes surface flow to a given NHD flowline. While this
may seem like a significant difference, in reality, the NHDPlus COMID identifier
lends itself very well to the HY\_Features catchment concept. The COMID is
used as an identifier for the catchment polygon, the flowline that
connects the catchment inlet and outlet, and value added attributes that
describe characteristics of the catchment's interior. In this way, the COMID
identifier is actually an identifier for a collection of data that
together fully describe an NHDPlus catchment. [See the NHDPlus mapping to
HY_Features in the HY_Features specification.](http://docs.opengeospatial.org/is/14-111r6/14-111r6.html#annexD_1)

Below is a description of the expected scope of data to be used by the package. 
While other data and attributes may come into scope,
it should only be done as a naive pass-through, as in data subsetting, or
with considerable deliberation.

##### Flowlines and Waterbodies
Flowline geometry is a mix of 1-d streams and 1-d "artificial paths". In order
to complete the set of features meant to represent water, we need to include
waterbody and potentially NHDArea polygons (double line stream overlays).

##### Catchment Polygons
Catchment polygons are the result of a complete elevation derived hydrography
process with hydro-enforcement applied with both Watershed Boundary Dataset
Hydrologic Units and NHD reaches.

##### Network Attributes
The NHDPlus includes numerous attributes that are built using the network and
allow a wide array of capabilities that would require excessive iteration or
sophisticated and complex graph-oriented data structures and algorithms.

##### Package Dependencies
If at all possible, dependencies should be available via CRAN, have solid
expected maintenance, allow national-scale analyses, and not require difficult
to install system libraries. `dplyr`, and `sf` are the primary dependencies that
should be used if at all possible.

### Contributing:

First, thanks for considering a contribution! I hope to make this package a community created resource
for us all to gain from and won't be able to do that without your help!

1) Contributions should be thoroughly tested with [testthat](https://testthat.r-lib.org/).  
2) Code style should attempt to follow the [tidyverse style guide.](http://style.tidyverse.org/)  
3) Please attempt to describe what you want to do prior to contributing by submitting an issue.  
4) Please follow the typical github [fork - pull-request workflow.](https://gist.github.com/Chaser324/ce0505fbed06b947d962)  
5) Make sure you use roxygen and run Check before contributing. More on this front as the package matures. 

Other notes:
- lintr runs in the tests so... write good code.
- consider running `goodpractice::gp()` on the package before contributing.
- consider running `devtools::spell_check()` if you wrote documentation.
- this package may end up using pkgdown running `pkgdown::build_site()` will refresh it.

## Disclaimer

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information.

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey  (USGS), an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [https://www.usgs.gov/visual-id/credit_usgs.html#copyright](https://www.usgs.gov/visual-id/credit_usgs.html#copyright)

Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

 [
    ![CC0](https://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](https://creativecommons.org/publicdomain/zero/1.0/)
