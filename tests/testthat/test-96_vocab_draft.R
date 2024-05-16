testthat::skip_on_cran()
testthat::skip_if_not_installed(pkg="quanteda")
testthat::skip_if_not_installed(pkg="udpipe")

tmp_path="test_data/language_models/udpipe_models/english-ewt-ud-2.5-191206.udpipe"
tmp_condition=file.exists(testthat::test_path(tmp_path))
testthat::skip_if_not(condition=tmp_condition,
                  message = "udpipe language model not available")

test_that("bow_pp_create_vocab_draft", {
  example_data<-imdb_movie_reviews

  res<-bow_pp_create_vocab_draft(
    path_language_model=testthat::test_path(tmp_path),
    data=example_data$text[1:150],
    upos=c("NOUN", "ADJ","VERB"),
    label_language_model="english-ewt-ud-2.5-191206",
    language="english",
    chunk_size=50,
    trace=FALSE)

expect_type(res,type="list")
})



