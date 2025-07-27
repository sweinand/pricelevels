# pricelevels 1.4.0

**Bug fixes:**

*	In `nlcpd_self_start(strategy="s1")` where the base region was not set properly.
*	In `pricelevels(settings=list(solve="matrix"))` where the setting was not supported and is now internally set to `iterative`.
*	In `nlcpd(settings=list(use.jac=TRUE)` when there were two regions (`R=2`) leading the matrices `B` and `C` in `nlcpd_jacobi()` to collapse.
Solved by adding `drop=FALSE` to the two matrices and added tests for this case.
*	In `lehr()`, when some quantities in the data were 0, the function returned `NaN`.
*	Fixed some links to the` minpack.lm`-package in the documentation for `cpd()` and `nlcpd()`.

**Improvements and other changes:**

* In `pricelevels(settings=list(type=c("a","b"")))`, no alphabetic ordering by index name of the output if specific indices are provided; alphabetic ordering only if `settings$type=NULL`.
* Added the function `geoyoung()` for the geometric Young index. Function description, tests and vignette updated.
* For `all.pairs=FALSE`, the functions `index.pairs()` and `geks()` no longer use the price levels where the comparison and base region are the same (e.g., $P^{11}$) as these price levels do not provide valuable interregional information.
* The attribute `attr(x, "base")` is always added to the function output of `ratios()` if `static=FALSE` and base is not `NULL`.
The attribute provides the base region used to calculate the individual price ratios for each product.
The treatment of duplicated prices by `ratios()` is now also aligned to the respective treatment by all index formulas in the package, that is, duplicated prices are averaged and ratios derived by dividing the initial prices by the average.
* Added the function `pairs()` to compute the number of non-redundant index pairs available for a data set.
Renamed the functions `comparisons()` and `sparsity()` to `properties()` and `gaps()`. 
Updated the output of `properties()`.
* Introduced the new (non-exported) setting `settings$prods`, which removes products with prices in only one region.
This is needed because otherwise these products distort the estimation by `cpd()`, `ikle()`, `gerardi()`, and others if weights are used.
* Only positive prices are supported.
Before, also zero prices were allowed, which cannot be processed by most index formulas, however.

# pricelevels 1.3.0

* Introduced the setting of global options `options(pricelevels.[])`
* Introduced the plotting of price levels using the new non-exported function `plot.pricelevels()`
* Adjusted the behavior of `ratios()`
* Fixed a bug in the non-exported function `arrange()`

# pricelevels 1.2.0

* Included the functions 
  - `mjevons()` in `rao()`
  - `mcarli()` in `rhajargsaht()`
  - `mdutot()` in `gkhamis()`
  - `mharmonic()` in `ikle()`
  
  by setting `q=NULL` and/or `w=NULL`
* Updated the package vignette
* Updated tests

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
