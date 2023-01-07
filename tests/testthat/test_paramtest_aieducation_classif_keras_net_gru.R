test_that("classif.keras_net_gru train", {
  learner = lrn("classif.keras_net_gru")
  fun = aieducation::keras_net_gru
  exclude = c(
    "object", # handled internally
    "data" # handled internally
  )

  # note that you can also pass a list of functions in case $.train calls more than one
  # function, e.g. for control arguments
  paramtest = run_paramtest(learner, fun, exclude, tag = "train")
  expect_paramtest(paramtest)
})

test_that("classif.keras_net_gru predict", {
  learner = lrn("classif.keras_net_gru")
  fun = aieducation::predict # nolint
  exclude = c(
    "object", # handled internally
    "data", # handled internally
    "newdata" # handled internally
  )

  paramtest = run_paramtest(learner, fun, exclude, tag = "predict")
  expect_paramtest(paramtest)
})
