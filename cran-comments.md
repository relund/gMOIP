## Test environments
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.6.1
* rhub (devel and release - Win Server, Ubuntu Linux, Fedora Linux)


## R CMD check results
R CMD check results
0 errors | 0 warnings | 1 note

√  checking for file 'gMOIP/DESCRIPTION'
-  checking extension type ... Package
-  this is package 'gMOIP' version '1.3.0' (814ms)
-  package encoding: UTF-8
N  checking CRAN incoming feasibility
   Maintainer: 'Lars Relund <lars@relund.dk>'
   
   Possibly mis-spelled words in DESCRIPTION:
     ILP (9:60)
     MILP (9:67)
     polytope (9:42)

R CMD check succeeded


## Downstream dependencies
* None

## Steps for realesing to CRAN
devtools::check()
devtools::spell_check()
devtools::check_rhub(interactive = FALSE)
devtools::check_win_devel()
devtools::release()
