# Spatial Index Numbers (spin)

## Introduction
Package `spin` provides methods which can be used for price comparisons between spatial entities. Its functions are designed based on `R`'s [`data.table`](https://github.com/Rdatatable/data.table) package.

Currently, the following index number methods are implemented:
1. Elementary price indices:  Jevons, Dutot, Carli, Harmonic
2. Weighted price indices:    Laspeyres, Paasche, Fisher, Toernqvist, Walsh
3. Multilateral price indices:  CPD, NLCPD, GEKS, Geary-Khamis, Iklé-Dikhanov-Balk, Geradi, Rao

Moreover, `spin` provides additional functionalities for sampling and characterizing (incomplete) price data.

## Working with Git

### Setting up Git
Guidelines for installing and setting up Git together with RStudio can be found, for example, [`here`](http://www.geo.uzh.ch/microsite/reproducible_research/post/rr-rstudio-git/). Connecting to the `spin` repository can be done in `RStudio` by clicking `File -> New Project -> Version Control -> Git`. The repository will be cloned to your local machine, and by virtue of it being a `RStudio` project, all relative directory paths will correctly resolve.

### Naming branches
To maintain a uniform naming scheme for branches, please regard the following rules. Branch names should be composed of two parts, separated by a `/`. First, one of `dev`, `bug`, `feat`, depending on whether you are `dev`eloping the code, fixing a `bug`, or implementing a new `feat`ure. Second, give a concise name with words separated by `_`.

**Examples**
* The branch `dev/update_geks` updates the code underlying the GEKS index.
* `bug/jevons_calculations` fixes a bug in the calculation of Jevons indices.

These are preliminary rules of 2020-05-22.
