# pricelevels 1.1.0

* New functions `mjevons()`, `mcarli()`, `mdutot()`, and `mharmonic()`
* In `nlcpd()`, introduced matching of lower and upper bounds and replaced argument `par.start` with the default `par`
* Separated the `gerardi()` index from the iterative ones
* Duplicated prices are aggregated now as weighted averages instead of unweighted ones in the helper function `arrange()`
* Updated the package vignette
* Updated tests

# pricelevels 1.0.2

In the DESCRIPTION file:

* Updated the package description
* Added a reference to the World Bank's methodological manual 
* Included the Authors@R field

# pricelevels 1.0.1

* Fixed the 'no visible bindings for global variable' note caused by the use of the data.table-package.
* Replaced `\dontrun{}` with `\donttest{}` in the examples.

# pricelevels 1.0.0

* Initial CRAN submission.
* Initial release on Github.
