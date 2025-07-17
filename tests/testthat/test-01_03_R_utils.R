test_that("get_n_chunks", {
  times=sample(x=seq.int(from=2,to=100,by=1),size=1)
  seq_len=sample(x=seq.int(from=1,to=times,by=1),size=10,replace = TRUE)
  features=sample(x=seq.int(from=162,to=1024,by=1),size=1)
  pad_value <- sample(x = seq(from = -200, to = 0, by = 10), size = 1)

  example_embeddings=generate_embeddings(
    times=times,
    features=features,
    seq_len=seq_len,
    pad_value=pad_value
  )

  calculated_times=get_n_chunks(
    text_embeddings = example_embeddings,
    times=times,
    features=features,
    pad_value=pad_value
  )

  expect_equal(seq_len,calculated_times)
})

test_that("get_file_extension", {
path_test_data=testthat::test_path("test_data/LargeDataSetForTexts/single_text")

#Txt files
files=list.files(path_test_data,full.names = TRUE,
                 pattern=".txt")
for(file in files){
  expect_equal(get_file_extension(file),"txt")
}

#Pdf files
files=list.files(path_test_data,full.names = TRUE,
                 pattern=".pdf")
for(file in files){
  expect_equal(get_file_extension(file),"pdf")
}

#Xlsx files
files=list.files(path_test_data,full.names = TRUE,
                 pattern=".xlsx")
for(file in files){
  expect_equal(get_file_extension(file),"xlsx")
}

})

test_that("tmp_dir", {
  expect_no_error(create_and_get_tmp_dir())
  expect_no_error(clean_tmp_dir())
})

test_that("get_alpha_3_codes", {
expect_vector(get_alpha_3_codes())
})




