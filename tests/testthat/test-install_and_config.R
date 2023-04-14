test_that("check_python_modules", {
  expect_type(invisible(check_aif_py_modules()),
              "logical")
})
