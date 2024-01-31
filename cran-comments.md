## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
```
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Sebastian Weinand <s.weinand90@googlemail.com>'
  
  New submission
```

❯ On windows-x86_64-devel (r-devel)
```
  checking R code for possible problems ... NOTE
  arrange: no visible binding for global variable 'ng'
  bilateral.index: no visible binding for global variable 'uses_q'
  bilateral.index: no visible binding for global variable 'uses_w'
  bilateral.index: no visible binding for global variable 'name'
  bilateral.index: no visible binding for global variable 'p_base'
  bilateral.index: no visible binding for global variable 'q_qbase'
  bilateral.index: no visible binding for global variable 'p_qbase'
  bilateral.index: no visible binding for global variable 'w_base'
  bilateral.index: no visible binding for global variable 'q_base'
  comparisons: no visible binding for global variable 'var1'
  comparisons: no visible binding for global variable 'var2'
  comparisons: no visible binding for global variable 'freq'
  geks.main: no visible binding for global variable 's'
  geks.main: no visible binding for global variable 's.x'
  geks.main: no visible binding for global variable 's.y'
  index.pairs: no visible binding for global variable 'uses_q'
  index.pairs: no visible binding for global variable 'uses_w'
  index.pairs: no visible binding for global variable 'name'
  min_obs: no visible binding for global variable 'r'
  min_obs: no visible binding for global variable 'n'
  nlcpd_self_start: no visible binding for global variable 'w_delta'
  nlcpd_self_start: no visible binding for global variable 'd'
  pricelevels: no visible binding for global variable 'uses_none'
  pricelevels: no visible binding for global variable 'name'
  pricelevels: no visible binding for global variable 'uses_w'
  pricelevels: no visible binding for global variable 'uses_q'
  ratios: no visible binding for global variable 'region'
  ratios: no visible binding for global variable 'price'
  ratios: no visible binding for global variable 'is_base'
  ratios: no visible binding for global variable 'region.y'
  ratios: no visible binding for global variable 'price.x'
  ratios: no visible binding for global variable 'price.y'
  ratios: no visible binding for global variable 'rid.x'
  ratios: no visible binding for global variable 'rid.y'
  rdata: no visible binding for global variable 'region'
  rdata: no visible binding for global variable 'group'
  rdata: no visible binding for global variable 'weight'
  rdata: no visible binding for global variable 'price'
  rdata: no visible binding for global variable 'error'
  rdata: no visible binding for global variable 'prod_share'
  rdata: no visible binding for global variable 'turnover'
  rdata: no visible binding for global variable 'prod_turnover'
  rdata: no visible binding for global variable 'quantity'
  rdata: no visible binding for global variable 'product'
  rweights: no visible binding for global variable 'w'
  rweights: no visible binding for global variable 'w_adj'
  Undefined global functions or variables:
    d error freq group is_base n name ng p_base p_qbase price price.x
    price.y prod_share prod_turnover product q_base q_qbase quantity r
    region region.y rid.x rid.y s s.x s.y turnover uses_none uses_q
    uses_w var1 var2 w w_adj w_base w_delta weight
```

These messages are due to the use of data.table within the functions and do not cause any problems.

❯ On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
```
  checking HTML version of manual ... NOTE
  Skipping checking math rendering: package 'V8' unavailable
```
This seems to be a problem on the test server.

❯ On windows-x86_64-devel (r-devel)
```
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''
```
As noted in [R-hub issue #560](https://github.com/r-hub/rhub/issues/560), this seems to be an Rhub issue and so can likely be ignored. 

❯ On windows-x86_64-devel (r-devel)
```
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
```
As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

❯ On ubuntu-gcc-release (r-release)
```
  checking CRAN incoming feasibility ... [5s/17s] NOTE
  Maintainer: ‘Sebastian Weinand <s.weinand90@googlemail.com>’
  
  New submission
```

❯ On ubuntu-gcc-release (r-release)
```
  checking R code for possible problems ... [10s/25s] NOTE
  arrange: no visible binding for global variable ‘ng’
  bilateral.index: no visible binding for global variable ‘uses_q’
  bilateral.index: no visible binding for global variable ‘uses_w’
  bilateral.index: no visible binding for global variable ‘name’
  bilateral.index: no visible binding for global variable ‘p_base’
  bilateral.index: no visible binding for global variable ‘q_qbase’
  bilateral.index: no visible binding for global variable ‘p_qbase’
  bilateral.index: no visible binding for global variable ‘w_base’
  bilateral.index: no visible binding for global variable ‘q_base’
  comparisons: no visible binding for global variable ‘var1’
  comparisons: no visible binding for global variable ‘var2’
  comparisons: no visible binding for global variable ‘freq’
  geks.main: no visible binding for global variable ‘s’
  geks.main: no visible binding for global variable ‘s.x’
  geks.main: no visible binding for global variable ‘s.y’
  index.pairs: no visible binding for global variable ‘uses_q’
  index.pairs: no visible binding for global variable ‘uses_w’
  index.pairs: no visible binding for global variable ‘name’
  min_obs: no visible binding for global variable ‘n’
  min_obs: no visible binding for global variable ‘r’
  nlcpd_self_start: no visible binding for global variable ‘w_delta’
  nlcpd_self_start: no visible binding for global variable ‘d’
  pricelevels: no visible binding for global variable ‘uses_none’
  pricelevels: no visible binding for global variable ‘name’
  pricelevels: no visible binding for global variable ‘uses_w’
  pricelevels: no visible binding for global variable ‘uses_q’
  ratios: no visible binding for global variable ‘region’
  ratios: no visible binding for global variable ‘price’
  ratios: no visible binding for global variable ‘is_base’
  ratios: no visible binding for global variable ‘region.y’
  ratios: no visible binding for global variable ‘price.x’
  ratios: no visible binding for global variable ‘price.y’
  ratios: no visible binding for global variable ‘rid.x’
  ratios: no visible binding for global variable ‘rid.y’
  rdata: no visible binding for global variable ‘region’
  rdata: no visible binding for global variable ‘group’
  rdata: no visible binding for global variable ‘weight’
  rdata: no visible binding for global variable ‘price’
  rdata: no visible binding for global variable ‘error’
  rdata: no visible binding for global variable ‘prod_share’
  rdata: no visible binding for global variable ‘turnover’
  rdata: no visible binding for global variable ‘prod_turnover’
  rdata: no visible binding for global variable ‘quantity’
  rdata: no visible binding for global variable ‘product’
  rweights: no visible binding for global variable ‘w’
  rweights: no visible binding for global variable ‘w_adj’
  Undefined global functions or variables:
    d error freq group is_base n name ng p_base p_qbase price price.x
    price.y prod_share prod_turnover product q_base q_qbase quantity r
    region region.y rid.x rid.y s s.x s.y turnover uses_none uses_q
    uses_w var1 var2 w w_adj w_base w_delta weight
```

These messages are due to the use of data.table within the functions and do not cause any problems.

❯ On fedora-clang-devel (r-devel)
```
  checking CRAN incoming feasibility ... [6s/21s] NOTE
  Maintainer: ‘Sebastian Weinand <s.weinand90@googlemail.com>’
  
  New submission
```

❯ On fedora-clang-devel (r-devel)
```
  checking R code for possible problems ... [11s/25s] NOTE
  arrange: no visible binding for global variable ‘ng’
  bilateral.index: no visible binding for global variable ‘uses_q’
  bilateral.index: no visible binding for global variable ‘uses_w’
  bilateral.index: no visible binding for global variable ‘name’
  bilateral.index: no visible binding for global variable ‘p_base’
  bilateral.index: no visible binding for global variable ‘q_qbase’
  bilateral.index: no visible binding for global variable ‘p_qbase’
  bilateral.index: no visible binding for global variable ‘w_base’
  bilateral.index: no visible binding for global variable ‘q_base’
  comparisons: no visible binding for global variable ‘var1’
  comparisons: no visible binding for global variable ‘var2’
  comparisons: no visible binding for global variable ‘freq’
  geks.main: no visible binding for global variable ‘s’
  geks.main: no visible binding for global variable ‘s.x’
  geks.main: no visible binding for global variable ‘s.y’
  index.pairs: no visible binding for global variable ‘uses_q’
  index.pairs: no visible binding for global variable ‘uses_w’
  index.pairs: no visible binding for global variable ‘name’
  min_obs: no visible binding for global variable ‘n’
  min_obs: no visible binding for global variable ‘r’
  nlcpd_self_start: no visible binding for global variable ‘w_delta’
  nlcpd_self_start: no visible binding for global variable ‘d’
  pricelevels: no visible binding for global variable ‘uses_none’
  pricelevels: no visible binding for global variable ‘name’
  pricelevels: no visible binding for global variable ‘uses_w’
  pricelevels: no visible binding for global variable ‘uses_q’
  ratios: no visible binding for global variable ‘region’
  ratios: no visible binding for global variable ‘price’
  ratios: no visible binding for global variable ‘is_base’
  ratios: no visible binding for global variable ‘region.y’
  ratios: no visible binding for global variable ‘price.x’
  ratios: no visible binding for global variable ‘price.y’
  ratios: no visible binding for global variable ‘rid.x’
  ratios: no visible binding for global variable ‘rid.y’
  rdata: no visible binding for global variable ‘region’
  rdata: no visible binding for global variable ‘group’
  rdata: no visible binding for global variable ‘weight’
  rdata: no visible binding for global variable ‘price’
  rdata: no visible binding for global variable ‘error’
  rdata: no visible binding for global variable ‘prod_share’
  rdata: no visible binding for global variable ‘turnover’
  rdata: no visible binding for global variable ‘prod_turnover’
  rdata: no visible binding for global variable ‘quantity’
  rdata: no visible binding for global variable ‘product’
  rweights: no visible binding for global variable ‘w’
  rweights: no visible binding for global variable ‘w_adj’
  Undefined global functions or variables:
    d error freq group is_base n name ng p_base p_qbase price price.x
    price.y prod_share prod_turnover product q_base q_qbase quantity r
    region region.y rid.x rid.y s s.x s.y turnover uses_none uses_q
    uses_w var1 var2 w w_adj w_base w_delta weight
```

These messages are due to the use of data.table within the functions and do not cause any problems.

0 errors ✔ | 0 warnings ✔ | 9 notes ✖
