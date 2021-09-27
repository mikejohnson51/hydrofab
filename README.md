# hyRefactor

[![R-CMD-check](https://github.com/dblodgett-usgs/hyRefactor/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/dblodgett-usgs/hyRefactor/actions/workflows/R-CMD-check.yml) [![codecov](https://codecov.io/gh/dblodgett-usgs/hyRefactor/branch/master/graph/badge.svg?token=GSJGAVH1T4)](https://codecov.io/gh/dblodgett-usgs/hyRefactor)

## Tools for Manipulating the NHDPlus Network in Preparation for Hydrologic Modeling

### Installation:

```
install.packages("devtools")
install.packages("rgeos", repos="http://R-Forge.R-project.org", type="source")
devtools::install_github("dblodgett-usgs/hyRefactor")
```

This package is based around the same concepts as [nhdplusTools](https://usgs-r.github.io/nhdplusTools/) and uses its utilities extensively.

### What is Refactoring in the context of hydrographic data?

The concept of refactoring as intended here includes:

1) **Splitting** large or long catchments to create a more uniform catchment size
distribution,  
2) **collapsing** catchment topology to eliminate small catchments,  
3) **aggregating** catchments into groups based on existing network topology.  

This type of functionality is especially relevant to modeling applications that
need specific modeling unit characteristics but wish to preserve the network as
much as possible for interoperability with other applications that use the
NHDPlus network.

### Check notes:
In addition to typical R package checking, a Dockerfile is included in this repository. Once built, it can be run with the following command.

```
docker build -t hyrefactor_test .

docker run --rm -it -v %cd%:/src hyrefactor_test /bin/bash -c "cp -r /src/* /check/ && cp /src/.Rbuildignore /check/ && cd /check && Rscript -e 'devtools::build()' && R CMD check --as-cran ../hyRefacto_*"
```

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
