## Test environments
* Mac OS (local), R (release)
* Mac OS (latest) on GitHub Actions, R (release)
* Windows (latest) on GitHub Actions, R (release)
* Ubuntu 16.04 on GitHub Actions, R (release, devel, oldrel)
* Win-builder (check_win_release)


## R CMD check results
R CMD check results
0 errors | 0 warnings | 1 note

checking installed package size ... NOTE
  installed size is  6.2Mb
  sub-directories of 1Mb or more:
    doc   4.9Mb
    
R CMD check succeeded


## Downstream dependencies
* None


## Steps for releasing to CRAN
devtools::spell_check()
devtools::check(env_vars = c(NOT_CRAN = "true", RGL_USE_NULL = "true"))
devtools::check_win_release(quiet = TRUE)
  # Push files to GitHub for GitHub actions check
devtools::release() 


