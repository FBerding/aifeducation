# Start time
test_time_start <- Sys.time()

test_that("get_n_chunks", {
  times <- sample(x = seq.int(from = 2, to = 100, by = 1), size = 1)
  seq_len <- sample(x = seq.int(from = 1, to = times, by = 1), size = 10, replace = TRUE)
  features <- sample(x = seq.int(from = 162, to = 1024, by = 1), size = 1)
  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)

  example_embeddings <- generate_embeddings(
    times = times,
    features = features,
    seq_len = seq_len,
    pad_value = pad_value
  )

  calculated_times <- get_n_chunks(
    text_embeddings = example_embeddings,
    times = times,
    features = features,
    pad_value = pad_value
  )

  expect_equal(seq_len, calculated_times)
})

test_that("get_file_extension", {
  path_test_data <- testthat::test_path("test_data/LargeDataSetForTexts/single_text")

  # Txt files
  files <- list.files(path_test_data,
    full.names = TRUE,
    pattern = ".txt"
  )
  for (file in files) {
    expect_equal(get_file_extension(file), "txt")
  }

  # Pdf files
  files <- list.files(path_test_data,
    full.names = TRUE,
    pattern = ".pdf"
  )
  for (file in files) {
    expect_equal(get_file_extension(file), "pdf")
  }

  # Xlsx files
  files <- list.files(path_test_data,
    full.names = TRUE,
    pattern = ".xlsx"
  )
  for (file in files) {
    expect_equal(get_file_extension(file), "xlsx")
  }
})

test_that("tmp_dir", {
  expect_no_error(create_and_get_tmp_dir())
  expect_no_error(clean_tmp_dir())
})

test_that("get_alpha_3_codes", {
  expect_vector(get_alpha_3_codes())
})

test_that("check_class", {
  expect_no_error(
    check_class(
      object = factor(x = c("a", "b", "b")),
      object_name = NULL,
      classes = "factor",
      allow_NULL = FALSE
    )
  )
  expect_error(
    check_class(
      object = factor(x = c("a", "b", "b")),
      object_name = NULL,
      classes = "BaseModelCore",
      allow_NULL = FALSE
    )
  )
  expect_no_error(
    check_class(
      object = NULL,
      object_name = NULL,
      classes = "BaseModelCore",
      allow_NULL = TRUE
    )
  )
  expect_error(
    check_class(
      object = NULL,
      object_name = NULL,
      classes = "BaseModelCore",
      allow_NULL = FALSE
    )
  )
})

test_that("check_type", {
  types <- c("bool", "int", "double", "(double", "double)", "(double)", "string", "vector", "list")
  objects <- list(
    "bool" = TRUE,
    "int" = 2L,
    "double" = 0.5,
    "(double" = 0.5,
    "double)" = 0.5,
    "(double)" = 0.5,
    "string" = "test_string",
    "vector" = c(1L, 0.5),
    "list" = list(a = 5, b = 10)
  )
  allow_null_vars <- c(TRUE, FALSE)
  for (type in types) {
    for (allow_null in allow_null_vars) {
      expect_no_error(
        check_type(
          object = objects[[type]],
          object_name = "test_object",
          type = type,
          allow_NULL = allow_null,
          min = 0L,
          max = 2L,
          allowed_values = NULL
        )
      )

      if (allow_null) {
        expect_no_error(
          check_type(
            object = NULL,
            object_name = "test_object",
            type = type,
            allow_NULL = allow_null,
            min = 0L,
            max = 2L,
            allowed_values = NULL
          )
        )
      } else {
        expect_error(
          check_type(
            object = NULL,
            object_name = "test_object",
            type = type,
            allow_NULL = allow_null,
            min = 0L,
            max = 2L,
            allowed_values = NULL
          )
        )
      }
    }
  }
})

test_that("inspect_tmp_dir", {
  suppressMessages({
    results <- inspect_tmp_dir()
  })
  expect_type(object = results, type = "list")
  expect_gte(results$cum_size, 0L)
})

# Monitor test time
monitor_test_time_on_CI(
  start_time = test_time_start,
  test_name = "00_01_03_R_utils"
)
