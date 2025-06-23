## Check package (run as background job)
update.packages(ask = FALSE, repos = "https://cran.rstudio.com/")
devtools::spell_check()
spelling::update_wordlist()
devtools::check(env_vars = c(NOT_CRAN = "true", RGL_USE_NULL = "true"))
# revdepcheck::revdep_check(num_workers = 4)
rhub::rhub_check(platforms = c("windows"))  # check using GitHub Actions
devtools::check_win_release(quiet = TRUE) # win-builder
devtools::check_win_devel(quiet = TRUE) # win-builder
# Push files to GitHub for GitHub actions check
# Submit to CRAN
# devtools::release()