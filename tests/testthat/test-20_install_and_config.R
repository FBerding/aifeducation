test_that("check_python_modules", {
  expect_type(
    check_aif_py_modules(trace = FALSE),
    "logical"
  )
})

test_that("check_python_modules", {
  expect_no_error(set_config_os_environ_logger(level = "ERROR"))
})
