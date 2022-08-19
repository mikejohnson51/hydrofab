
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydrofab: Fabricating Hydrofabrics <img src='man/figures/imgfile.png' align="right" height="139" />

<!-- badges: start -->

[![Dependencies](https://img.shields.io/badge/dependencies-19/80-red?style=flat)](#)
<!-- badges: end -->

The goal of `hydrofab` is to provide consistent hydrologic and hydraulic
network manipulation tool chains to achieve model application ready
datasets from a consistent reference fabric. Some of these are being
built at [ngen.hydrofab](https://github.com/mikejohnson51/ngen.hydrofab)
and [gfv2.0](https://code.usgs.gov/wma/nhgf/gfv2.0)

![Fabricating
Hydrofabrics](https://media1.giphy.com/media/Kd5uWrNKjhRGfopdHJ/giphy.gif?cid=ecf05e476u6oq5neboo6x6lk5gngbz2yuh5st0sxplbmczft&rid=giphy.gif)

## Installation

You can install the development version of hydrofab like so:

    install.packages("remotes")
    remotes::install_github("mikejohnson51/hydrofab")

Soon to be …

    install.packages("remotes")
    remotes::install_github("NOAA-OWP/hydrofab")

## Introduction

This package is based around the same concepts as
[nhdplusTools](https://usgs-r.github.io/nhdplusTools/) and uses its
utilities extensively. It is the creation of merging the previous
`hyRefactor` and `hyAggregate` packages in an attempt to provide a
common software stack of fabricating hydrofabrics.

### Refactoring and Aggregating

The concept of refactoring as intended here includes:

1)  **Splitting** large or long catchments to create a more uniform
    catchment size distribution,  
2)  **collapsing** catchment topology to eliminate small catchments

The concept of aggregating as intended here includes **aggregating**
catchments into groups based on existing network topology and defined
criteria. Two primary use cases are offered:

1.  Aggregating to a set of defined outlet locations
2.  Aggregating to a uniform catchment size with enforced minimum areas
    and lengths.

This type of functionality is especially relevant to modeling
applications that need specific modeling unit characteristics but wish
to preserve the network as much as possible for interoperability with
other applications

### Check notes:

In addition to typical R package, a Dockerfile is included in this
repository. Once built, it can be run with the following command.

    docker build -t hydrofab_test .

    # Linux/Mac

    docker run --rm -it -v $PWD:/src hydrofab_test /bin/bash -c "cp -r /src/* /check/ && cp /src/.Rbuildignore /check/ && cd /check && Rscript -e 'devtools::build(vignettes = FALSE)' && R CMD check --as-cran ../hydrofab_*"

    # Windows
    docker run --rm -it -v %cd%:/src hydrofab_test /bin/bash -c "cp -r /src/* /check/ && cp /src/.Rbuildignore /check/ && cd /check && Rscript -e 'devtools::build(vignettes = FALSE)' && R CMD check --as-cran ../hydrofab_"
