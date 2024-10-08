## Test environments
* Mac OS (local), R (release)
* Mac OS (latest) on GitHub Actions, R (release)
* Windows (latest) on GitHub Actions, R (release)
* Ubuntu (latest) on GitHub Actions, R (release, devel)
* Win-builder (check_win_release)
* Rhub (windows-x86_64-release)


## R CMD check results
R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
 

## Comments submission

* Added a new function.


## Downstream dependencies
* None


## Steps for releasing to CRAN
```r
update.packages(ask = FALSE)
devtools::spell_check()
spelling::update_wordlist()
devtools::check(env_vars = c(NOT_CRAN = "true", RGL_USE_NULL = "true"))
# revdepcheck::revdep_check(num_workers = 4)
devtools::check_rhub(platforms = c("windows-x86_64-release"), interactive = F)
devtools::check_win_release(quiet = TRUE) # win-builder
devtools::check_win_devel(quiet = TRUE) # win-builder
# Push files to GitHub for GitHub actions check
# Submit to CRAN
devtools::release()
```

