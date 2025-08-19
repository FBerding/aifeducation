testthat::skip_on_cran()

testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

test_that("check_python_modules", {
  expect_type(
    check_aif_py_modules(trace = FALSE),
    "logical"
  )
})

test_that("set_transformers_logger", {
  for(state in c("ERROR","WARNING","INFO","DEBUG")){
    expect_no_error(set_transformers_logger(level=state))
  }
  set_transformers_logger("WARNING")

})

test_that("prepare_session", {
expect_no_error(prepare_session())
})
