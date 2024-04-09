
testthat::skip_on_os("windows")
testthat::skip_if_not_installed(pkg="quanteda")
testthat::skip_if_not_installed(pkg="topicmodels")
testthat::skip_if_not_installed(pkg="text2vec")
testthat::skip_if_not_installed(pkg="tidytext")

path="test_data/gvc_lda/vocab_draft_movie_review.rda"
testthat::skip_if_not(condition=file.exists(testthat::test_path(path)),
                      message  = "Necessary dataset not available")
#------------------------------------------------------------------------------
load(testthat::test_path(path))

#------------------------------------------------------------------------------
test_that("bow_pp_create_vocab_draft", {

  example_data<-imdb_movie_reviews

  res<-bow_pp_create_basic_text_rep(
     data=example_data$text[1:100],
     vocab_draft=vocab_draft_movie_review,
     remove_punct = TRUE,
     remove_symbols = TRUE,
     remove_numbers = TRUE,
     remove_url = TRUE,
     remove_separators = TRUE,
     split_hyphens = FALSE,
     split_tags = FALSE,
     language_stopwords="de",
     use_lemmata = FALSE,
     to_lower=FALSE,
     min_termfreq = NULL,
     min_docfreq= NULL,
     max_docfreq=NULL,
     window = 5,
     weights = 1 / (1:5),
     trace=FALSE)

  expect_s3_class(res,class="basic_text_rep")
})



