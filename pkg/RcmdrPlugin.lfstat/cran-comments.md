## Test environments
* local debian testing (using deb jessie-cran3), R 3.2.2
* local Win7 install, R 3.2.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, no WARNINGs and one NOTE when running R CMD check locally. 

Maintainer: 'Tobias Gauster <t.gauster@boku.ac.at>'

New maintainer:
  Tobias Gauster <t.gauster@boku.ac.at>
Old maintainer(s):
  Gregor Laaha <gregor.laaha@boku.ac.at>
  
This is correct. t.gauster@boku.ac.at is going to be the new maintainer. This was annouced by gregor.laaha@boku.ac.at (On 29.09.2015 11:10) per mail to CRAN@r-project.org.


win-builder reported: 
Possibly mis-spelled words in DESCRIPTION:
  Rcmdr (3:8, 11:26)
  lfstat (11:55)
  
These are not mis-spelled: 
* WMO is the abbreviation of World Meteorological Organisation 
* lfstat is the package name of the upstream dependecy
* checking CRAN incoming feasibility ... NOTE


  
## Downstream dependencies
There are no downstream dependencies. 

## Upstream dependency 
This package depends on lfstat (>= 0.9.1) which was also submitted today.
