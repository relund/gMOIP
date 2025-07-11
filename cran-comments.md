## Test environments
* Mac OS (local), R (release)
* Mac OS (latest) on GitHub Actions, R (release)
* Windows (latest) on GitHub Actions, R (release)
* Ubuntu (latest) on GitHub Actions, R (release, devel)
* Win-builder (check_win_release)
* Rhub on GitHub actions (windows)


## R CMD check results
R CMD check results (local)

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


## revdepcheck results

NA
 

## Comments submission

* Fixed non-standard license specification.


## Downstream dependencies
* None


## Steps for releasing to CRAN
```r
update.packages(ask = FALSE)
devtools::spell_check()
spelling::update_wordlist()
devtools::check(env_vars = c(NOT_CRAN = "true", RGL_USE_NULL = "true"))
# revdepcheck::revdep_check(num_workers = 4)
rhub::rhub_check(platforms = c("windows"))
devtools::check_win_release(quiet = TRUE) # win-builder
devtools::check_win_devel(quiet = TRUE) # win-builder
# Push files to GitHub for GitHub actions check
# Submit to CRAN
devtools::release()
```

