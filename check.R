## Check package
update.packages()
devtools::spell_check()
spelling::update_wordlist()
devtools::check(env_vars = c(NOT_CRAN = "true", RGL_USE_NULL = "true"))
devtools::check_rhub(platforms = c("windows-x86_64-release"), interactive = F)
devtools::check_win_release(quiet = TRUE) # win-builder
# Push files to GitHub for GitHub actions check

devtools::check_rhub(interactive = F)
# Push files to GitHub for GitHub actions check
devtools::release()

# Submit to CRAN
devtools::release()
