## Test environments
* Mac OS (local), R (release)
* Mac OS (latest) on GitHub Actions, R (release)
* Windows (latest) on GitHub Actions, R (release)
* Ubuntu (latest) on GitHub Actions, R (release, devel)
* Win-builder (check_win_release)


## R CMD check results
R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔


## Downstream dependencies
* None


## Steps for releasing to CRAN
```
update.packages(ask = FALSE)
devtools::spell_check()
spelling::update_wordlist()
devtools::check(env_vars = c(NOT_CRAN = "true", RGL_USE_NULL = "true"))
devtools::check_rhub(interactive = F)
devtools::check_win_release(quiet = TRUE) # win-builder
# Push files to GitHub for GitHub actions check
# Submit to CRAN
devtools::release()
```

