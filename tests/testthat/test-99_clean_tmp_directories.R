testthat::skip_on_cran()

#Start time
test_time_start=Sys.time()

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

#Monitor test time
monitor_test_time_on_CI(
  start_time=test_time_start,
  test_name="99_clean_tmp_directories"
)
