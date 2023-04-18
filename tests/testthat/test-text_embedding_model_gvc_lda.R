example_data<-data.frame(
  id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id1,
  label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)


global_vector_clusters_modeling<-TextEmbeddingModel$new(
  model_name="global_vector_clusters_embedding",
  model_label="Text Embedding via Clusters of GlobalVectors",
  model_version="0.0.1",
  model_language="english",
  method="glove_cluster",
  bow_basic_text_rep=basic_text_rep_movie_reviews,
  bow_n_dim=25,
  bow_n_cluster=100,
  bow_max_iter=10,
  bow_max_iter_cluster=200,
  bow_cr_criterion=1e-8,
  trace=TRUE)

test_that("creation_GlobalVectorCluster", {
  expect_s3_class(global_vector_clusters_modeling,
                  class="TextEmbeddingModel")
})

test_that("embedding_GlobalVectorCluster", {
  embeddings<-global_vector_clusters_modeling$embed(raw_text = example_data$text[1:10],
                                        doc_id = 1:10)
  expect_s3_class(embeddings, class="EmbeddedText")
})

test_that("encoding_GlobalVectorCluster", {
  encodings<-global_vector_clusters_modeling$encode(raw_text = example_data$text[1:10])
  expect_length(encodings,10)
  expect_type(encodings,type="list")
})

test_that("decoding_GlobalVectorCluster", {
  encodings<-global_vector_clusters_modeling$encode(raw_text = example_data$text[1:10])
  decodings<-global_vector_clusters_modeling$decode(encodings)
  expect_length(decodings,10)
  expect_type(decodings,type="list")
})


#------------------------------------------------------------------------------
topic_modeling<-TextEmbeddingModel$new(
  model_name="topic_model_embedding",
  model_label="Text Embedding via Topic Modeling",
  model_version="0.0.1",
  model_language="english",
  method="lda",
  bow_basic_text_rep=basic_text_rep_movie_reviews,
  bow_n_dim=2,
  bow_max_iter=10,
  bow_cr_criterion=1e-8,
  trace=FALSE)


test_that("creation_topic_modeling", {
  expect_s3_class(topic_modeling,
                  class="TextEmbeddingModel")
})

test_that("embedding_topic_modeling", {
  embeddings<-topic_modeling$embed(raw_text = example_data$text[1:10],
                                   doc_id = 1:10)
  expect_s3_class(embeddings, class="EmbeddedText")
})

test_that("encoding_topic_modeling", {
  encodings<-topic_modeling$encode(raw_text = example_data$text[1:10])

  expect_length(encodings,10)
  expect_type(encodings,type="list")
})

test_that("decoding_topic_modeling", {
  encodings<-topic_modeling$encode(raw_text = example_data$text[1:10])
  decodings<-topic_modeling$decode(encodings)

  expect_length(decodings,10)
  expect_type(decodings,type="list")
})
