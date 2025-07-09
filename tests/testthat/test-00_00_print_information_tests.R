testthat::skip_on_cran()

testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

if (Sys.getenv("CI") == "true") {
  cat("\n")
  cat("---------------------------------------------------------\n")
  cat("On Continuous Integreation\n")
  cat("---------------------------------------------------------\n")
} else {
  cat("\n")
  cat("---------------------------------------------------------\n")
  cat("Not On Continuous Integreation\n")
  cat("---------------------------------------------------------\n")
}

# Print python versions of the test system
if (Sys.getenv("CI") == "true") {
  prepare_session(env_type ="conda",env_type = "r-reticulate")
} else {
  prepare_session()
}

#print(get_py_package_versions())

