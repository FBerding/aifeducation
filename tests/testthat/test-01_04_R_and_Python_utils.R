testthat::skip_on_cran()

testthat::skip_if_not(
  condition = check_aif_py_modules(trace = FALSE),
  message = "Necessary python modules not available"
)

test_that("class_vector_to_py_dataset", {
  vec=c(0,0,1,1,2,0)
  name_vector=c("A","B","C","d","e","f")
  names(vec)=name_vector
  dataset=class_vector_to_py_dataset(vec)

  expect_s3_class(object=dataset,class="datasets.arrow_dataset.Dataset")
  expect_equal(dataset[["id"]],name_vector)
  expect_equal(dataset[["labels"]],unname(vec))

})



