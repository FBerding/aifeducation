# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(aifeducation)

if (Sys.getenv("CI") == "true") {
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

test_check("aifeducation")
