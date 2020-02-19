## Test environments
* Ubuntu (xenial on Travis CI), R (release)
* Ubuntu (bionic on Travis CI), R (release)
* Mac OSX (newest stable version on Travis CI), R (release)
* Win-builder (check_win_release)


## R CMD check results
R CMD check results
0 errors | 0 warnings | 1 note

Checking CRAN incoming feasibility ... NOTE
Maintainer: 'Lars Relund Nielsen <lars@relund.dk>'

New maintainer:
  Lars Relund Nielsen <lars@relund.dk>
Old maintainer(s):
  Lars Relund <lars@relund.dk>

R CMD check succeeded


## Downstream dependencies
* None


## Steps for releasing to CRAN
devtools::spell_check()
devtools::check(run_dont_test = TRUE)
devtools::check_rhub(platforms = "windows-x86_64-release", interactive = F)
devtools::check_win_release(quiet = TRUE)
 # Push files to GitHub for TravisCI
devtools::release()

