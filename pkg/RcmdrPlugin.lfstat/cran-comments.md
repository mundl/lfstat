## Test environments
* local debian testing (using deb jessie-cran3), R 3.2.2
* local Win7 install, R 3.2.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE and 1 WARNING:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Tobias Gauster <t.gauster@boku.ac.at>'

New maintainer:
  Tobias Gauster <t.gauster@boku.ac.at>
Old maintainer(s):
  Gregor Laaha <gregor.laaha@boku.ac.at>
  
This is correct. t.gauster@boku.ac.at is going to be the new maintainer. This was annouced by gregor.laaha@boku.ac.at (On 29.09.2015 11:10) per mail to CRAN@r-project.org.



* checking for code which exercises the package ... WARNING
No examples, no tests, no vignettes

This is just a GUI for another package (lfstat). May get fixed within the following months.


  
## Downstream dependencies
There are no downstream dependencies. 
