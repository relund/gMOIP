## Check package
devtools::spell_check()
spelling::update_wordlist()
Yesdevtools::check(env_vars = c(NOT_CRAN = "true", RGL_USE_NULL = "true"))
devtools::check_rhub(platforms = c("windows-x86_64-release"), interactive = F) # is down
devtools::check_win_release(quiet = TRUE)
# Push files to GitHub for GitHub actions check

# Submit to CRAN
devtools::release()
