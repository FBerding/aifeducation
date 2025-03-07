testthat::skip_on_cran()

testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

if(Sys.getenv("CI")=="true"){
  print("---------------------------------------------------------")
  print("On Continuous Integreation")
  print("---------------------------------------------------------")
} else {
  print("---------------------------------------------------------")
  print("Not On Continuous Integreation")
  print("---------------------------------------------------------")
}

#Print python versions of the test system
print(get_py_package_versions())
