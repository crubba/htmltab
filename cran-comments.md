# Resubmission
This is a resubmission. In this version I have:

* Converted the DESCRIPTION title to title case
* Included call to person() in Authors@R field
* Confirmed that the package was checked with R-devel through win-builder

## Test environments
* local OS X install, R 3.1.2
* local Ubuntu 12.04, R 3.1.2
* win-builder (devel and release)

## R CMD check results
Local R CMD CHECKs succeeded with no ERRORs, WARNINGs or NOTEs. 

I experienced two NOTEs, when using devtools::check(args="--as-cran"), but not when I checked the package as usual:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Christian Rubba <christian.rubba@gmail.com>’
  New submission
  Components with restrictions and base license permitting such:
  MIT + file LICENSE
  File 'LICENSE':
  YEAR: 2014,2015
  COPYRIGHT HOLDER: Christian Rubba
  
* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
  
I experienced two NOTEs on R CMD CHECK on win-builder:

On release I got:

*Components with restrictions and base license permitting such:
  MIT + file LICENSE 
  File 'LICENSE':
  YEAR: 2014,2015
  COPYRIGHT HOLDER: Christian Rubba
  Possibly mis-spelled words in DESCRIPTION:
  htmltab (6:14)
  preprocesses (11:48)
  
Additionally, only on R-devel I got:

* checking R code for possible problems ... NOTE
  htmltab: possible error in check_type(doc = doc, which = which, ...):
  ... used in a situation where it does not exist

## Downstream dependencies
There are currently no downstream dependencies of this package
