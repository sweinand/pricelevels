## Resubmission
This is a resubmission. In this version I have:

* Introduced the setting of global options `options(pricelevels.[])`
* Introduced the plotting of price levels using the new non-exported function `plot.pricelevels()`
* Adjusted the behavior of `ratios()`
* Fixed a bug in the non-exported function `arrange()`

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
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

0 errors ✔ | 0 warnings ✔ | 3 notes ✖
