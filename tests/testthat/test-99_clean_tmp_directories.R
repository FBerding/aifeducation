testthat::skip_on_cran()

test_that("Clean tmp directories", {
  tmp_data <- testthat::test_path("test_data_tmp")
  tmp_artefacts <- testthat::test_path("test_artefacts")

  unlink(
    x = tmp_data,
    recursive = TRUE
  )
  unlink(
    x = tmp_artefacts,
    recursive = TRUE
  )

  expect_false(
    dir.exists(tmp_data)
  )
  expect_false(
    dir.exists(tmp_artefacts)
  )
})
