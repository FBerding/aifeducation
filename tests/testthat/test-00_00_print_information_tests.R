testthat::skip_on_cran()

testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

if (Sys.getenv("CI") == "true") {
  cat("---------------------------------------------------------\n")
  cat("On Continuous Integreation\n")
  cat("---------------------------------------------------------\n")
} else {
  cat("---------------------------------------------------------\n")
  cat("Not On Continuous Integreation\n")
  cat("---------------------------------------------------------\n")
}

# Print python versions of the test system
print(get_py_package_versions())
