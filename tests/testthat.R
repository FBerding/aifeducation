# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(aifeducation)

if(Sys.getenv("CI")=="true"){
  print("Env Variables---------------------------------------------------------")
  print("On Continuous Integreation")
  print("Env Variables---------------------------------------------------------")
} else {
  print("Env Variables---------------------------------------------------------")
  print("Not On Continuous Integreation")
  print("Env Variables---------------------------------------------------------")
}

test_check("aifeducation")
