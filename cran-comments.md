## Resubmission
This is a resubmission. In this version I have:

* Added the functions `mjevons()`, `mcarli()`, `mdutot()`, and `mharmonic()`
* Introduced matching of lower and upper bounds in `nlcpd()`
* Replaced argument `par.start` with the default `par` in `nlcpd()`
* Separated the `gerardi()` index from the iterative ones
* Duplicated prices are aggregated now as weighted averages instead of unweighted ones
* Updated the package vignette
* Updated tests

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
```
  checking HTML version of manual ... NOTE
  Skipping checking math rendering: package 'V8' unavailable
```
This seems to be a problem on the test server.

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
```
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found
  Skipping checking math rendering: package 'V8' unavailable
```
This seems to be a problem on the test servers.

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

0 errors ✔ | 0 warnings ✔ | 4 notes ✖
