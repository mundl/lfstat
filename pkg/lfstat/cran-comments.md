## Test environments
* local debian testing (using deb jessie-cran3), R 3.3.1
* local Win7 install, R 3.3.0
* local x86_64-apple-darwin.13.4.0 R 3.2.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, no WARNINGs and no NOTES when running R CMD check locally. 

win-builder reported: 
Possibly mis-spelled words in DESCRIPTION: WMO (9:44)

This is not mis-spelled: WMO is the abbreviation of World Meteorological Organisation 
  
  
## Downstream dependencies
 * RcmdrPlugin.lfstat: one WARNING: "No examples, no tests, no vignettes" warning is not related to upstream changes

