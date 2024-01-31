# Spatial price level compariosns (`pricelevels`)

The `pricelevels`-package provides index number methods for price comparisons within or between countries. As price comparisons over time usually rely on the same index number methods, the package has been denoted more generally as `pricelevels`, though its primary focus is on spatial price comparisons. Currently, the following index number methods (or price indices) are implemented.

1. **Bilateral price indices**:
    - Elementary (unweighted) indices:
      * Jevons: `jevons()`
      * Dutot: `dutot()`
      * Carli: `carli()`
      * Harmonic: `harmonic()`
      * CSWD: `cswd()`
    - Unit value related indices:
      * Unit value index: `uvalue()`
      * Banerjee: `banerjee()`
      * Davies: `davies()`
      * Lehr: `lehr()`
    - Quantity or expenditure share weighted indices:
      * (Geometric) Laspeyres: `laspeyres()` and `geolaspeyres()`
      * (Geometric) Paasche: `paasche()` and `geopaasche()`
      * Fisher: `fisher()`
      * Toernqvist: `toernqvist()`
      * (Geometric) Walsh: `walsh()` and `geowalsh()`
      * Theil: `theil()`
      * Marshall-Edgeworth: `medgeworth()`
      * Palgrave: `palgrave()`
      * Sato-Vartia: `svartia()`
      * Drobisch: `drobisch()`
      * Lowe: `lowe()`
      * Young: `young()`
2. **Multilateral price indices**: 
    - CPD and NLCPD methods: `cpd()` and `nlcpd()`
    - GEKS method: `geks()`
    - Multilateral systems of equations:
      * Geary-Khamis: `gkhamis()`
      * Iklé-Dikhanov-Balk: `idb()`
      * Gerardi: `gerardi()`
      * Rao: `rao()`

Moreover, the package offers functions for sampling and characterizing price data. Details can be found in the package vignette.

The `pricelevels`-functions are designed based on `R`'s [`data.table`](https://github.com/Rdatatable/data.table) package.
