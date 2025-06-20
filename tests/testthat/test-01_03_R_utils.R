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
