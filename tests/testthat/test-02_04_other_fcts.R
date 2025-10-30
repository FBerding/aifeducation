testthat::skip_on_cran()
testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

#Start time
test_time_start=Sys.time()

# Load python scripts
load_all_py_scripts()

test_that("CosineDistance", {
  device <- ifelse(torch$cuda$is_available(), "cuda", "cpu")
  base_tensor <- torch$from_numpy(
    reticulate::np_array(
      matrix(
        data = c(
          0, 1,
          1, 0,
          0, -1,
          -1, 0
        ),
        nrow = 4,
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  distance <- tensor_to_numpy(
    py$CosineDistance(
      x = base_tensor$to(device),
      y = base_tensor$to(device)
    )
  )
  expect_equal(
    tensor_to_numpy(distance),
    matrix(
      data = c(
        0, 1, 2, 1,
        1, 0, 1, 2,
        2, 1, 0, 1,
        1, 2, 1, 0
      ),
      nrow = 4,
      ncol = 4,
      byrow = TRUE
    )
  )
})

#Monitor test time
monitor_test_time_on_CI(
  start_time=test_time_start,
  test_name="02_04_other_functions"
)
