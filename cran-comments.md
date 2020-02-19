## Test environments
* Ubuntu (newest stable version on Travis CI), R (release)
* Mac OSX (newest stable version on Travis CI), R (release)
* Rhub (windows-x86_64-release)
* Win-builder (check_win_release)


## R CMD check results
R CMD check results
0 errors | 0 warnings | 0 notes

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

