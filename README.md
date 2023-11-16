# Spatial Index Number Methods (spin)

The `spin`-package provides index number methods for price comparisons within a country. As price comparisons between countries or other geographical entities usually rely on the same index number methods, the package has been denoted more generally as `spin` (for spatial index number methods). Currently, the following index number methods (or price indices) are implemented.

1. Bilateral price indices:
    - Non-weighted: Jevons, Dutot, Carli, Harmonic, CSWD
    - Weighted: Laspeyres, Paasche, Fisher, Toernqvist, Walsh, Theil, Marshall-Edgeworth, Palgrave, Sato-Vartia, Drobisch
2. Multilateral price indices: 
    - CPD and NLCPD methods
    - (Weighted) GEKS method
    - Multilateral systems of equations: Geary-Khamis, Iklé-Dikhanov-Balk, Geradi, Rao

Moreover, the package offers functions for sampling and characterizing price data. Details can be found in the package vignette.

The `spin`-functions are designed based on `R`'s [`data.table`](https://github.com/Rdatatable/data.table) package.
