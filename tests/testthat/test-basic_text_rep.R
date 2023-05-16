
path="test_data/vocab_draft_movie_review.rda"
testthat::skip_if_not(condition=file.exists(path),
                      message  = "Necessary dataset not available")
#------------------------------------------------------------------------------
load(path)

#------------------------------------------------------------------------------
test_that("bow_pp_create_vocab_draft", {

  example_data<-data.frame(
    id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id1,
    label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
  example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)

  res<-bow_pp_create_basic_text_rep(
     data=example_data$text,
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



