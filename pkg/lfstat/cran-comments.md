## Test environments
* local debian testing (using deb jessie-cran3), R 3.3.1
* local Win7 install, R 3.3.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, no WARNINGs and no NOTES when running R CMD check locally. 

win-builder reported: 
Possibly mis-spelled words in DESCRIPTION: WMO (9:44)

This is not mis-spelled: WMO is the abbreviation of World Meteorological Organisation 
  
  
## Downstream dependencie RcmdrPlugin.lfstat: 
 * There were no ERRORs, no WARNINGs and no NOTES when running R CMD check locally for RcmdrPlugin.lfstat.
 * RcmdrPlugin.lfstat will also be submitted to CRAN today and depends on lfstat (>= 0.9.1) which is only satisfied with the current submission of lfstat. 


## Release Frequency
Package lfstat version 0.9.0 was released to CRAN on 2016-07-01. The current release 0.9.4 became necessary as the former maintainer of the downstream dependency RcmdrPlugin.lfstat somehow mixed imports/exports of the two packages. Fixing all open issues of the downstream dependency required the very frequent rerelease of lfstat. Please excuse. 
