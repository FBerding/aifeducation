testthat::skip_on_cran()
testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

# Load python scripts
load_all_py_scripts()

test_that("CosineDistance", {
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
      x = base_tensor,
      y = base_tensor
    )
  )
  expect_equal(
    distance,
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
