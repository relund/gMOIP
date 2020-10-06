## Test environments
* Windows 10 (local), R (release)
* Ubuntu (xenial on Travis CI), R (release)
* Ubuntu (bionic on Travis CI), R (release, devel)
* RHub (windows-x86_64-release)
* Win-builder (check_win_release)


## R CMD check results
R CMD check results
0 errors | 0 warnings | 0 note

R CMD check succeeded


## Downstream dependencies
* None


## Steps for releasing to CRAN
devtools::spell_check()
devtools::check(run_dont_test = TRUE)
devtools::check_rhub(platforms = c("windows-x86_64-release"), interactive = F)
devtools::check_win_release(quiet = TRUE)
 # Push files to GitHub for TravisCI and check results 
devtools::release()

