## Test environments
* Mac OS (local), R (release)
* Mac OS (latest) on GitHub Actions, R (release)
* Windows (latest) on GitHub Actions, R (release)
* Ubuntu 20.04 on GitHub Actions, R (release, devel)
* Win-builder (check_win_release)


## R CMD check results
R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


## Downstream dependencies
* None


## Steps for releasing to CRAN
devtools::spell_check()
devtools::check(env_vars = c(NOT_CRAN = "true", RGL_USE_NULL = "true"))
devtools::check_win_release(quiet = TRUE)
  # Push files to GitHub for GitHub actions check
devtools::release() 


