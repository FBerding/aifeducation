test_that("check_python_modules", {
  expect_type(check_aif_py_modules(trace = FALSE),
              "logical")
})

