## Check package
# devtools::spell_check()
# devtools::check(run_dont_test = TRUE)
devtools::check(run_dont_test = FALSE)
devtools::check_rhub(platforms = c("windows-x86_64-release"), interactive = F) # is down
devtools::check_win_release(quiet = TRUE)
# Push files to GitHub for GitHub actions check


# Submit to CRAN
#devtools::release()