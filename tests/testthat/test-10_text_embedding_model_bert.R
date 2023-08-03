
testthat::skip_on_cran()
testthat::skip_if_not(condition=check_aif_py_modules(trace = FALSE),
                      message = "Necessary python modules not available")

tmp_path="test_data/bert"
testthat::skip_if_not(condition=dir.exists(testthat::test_path(tmp_path)),
                      message = "Necessary bert model not available")

if(dir.exists(testthat::test_path("tmp_full_models"))==FALSE){
  dir.create(testthat::test_path("tmp_full_models"))
}

if(dir.exists(testthat::test_path("tmp"))==FALSE){
  dir.create(testthat::test_path("tmp"))
}

aifeducation::set_config_gpu_low_memory()

#-------------------------------------------------------------------------------
example_data<-data.frame(
  id=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$id2,
  label=quanteda::docvars(quanteda.textmodels::data_corpus_moviereviews)$sentiment)
example_data$text<-as.character(quanteda.textmodels::data_corpus_moviereviews)

#-------------------------------------------------------------------------------
bert_modeling<-TextEmbeddingModel$new(
  model_name="bert_embedding",
  model_label="Text Embedding via BERT",
  model_version="0.0.1",
  model_language="english",
  method = "bert",
  max_length = 256,
  chunks=4,
  overlap=40,
  aggregation="last",
  model_dir=testthat::test_path(tmp_path))

model_name=bert_modeling$get_model_info()$model_name
model_name_root=bert_modeling$get_model_info()$model_name_root

test_that("creation_bert", {
  expect_s3_class(bert_modeling,
                  class="TextEmbeddingModel")
})

test_that("Saving Model Bert", {
  expect_no_error(
    bert_modeling$save_model(testthat::test_path("tmp/bert"))
  )
})

test_that("Loading Model Bert", {
  expect_no_error(
    bert_modeling$load_model(testthat::test_path("tmp/bert"))
  )
})

test_that("embedding_bert", {
  embeddings<-bert_modeling$embed(raw_text = example_data$text[1:10],
                                        doc_id = example_data$id[1:10])
  expect_s3_class(embeddings, class="EmbeddedText")

  embeddings<-NULL
  embeddings<-bert_modeling$embed(raw_text = example_data$text[1:1],
                                  doc_id = example_data$id[1:1])
  expect_s3_class(embeddings, class="EmbeddedText")

})

test_that("encoding_bert", {
  encodings<-bert_modeling$encode(raw_text = example_data$text[1:10],
                                  token_encodings_only = TRUE)
  expect_length(encodings,10)
  expect_type(encodings,type="list")
})

test_that("decoding_bert", {
  encodings<-bert_modeling$encode(raw_text = example_data$text[1:10],
                                  token_encodings_only = TRUE)
  decodings<-bert_modeling$decode(encodings)
  expect_length(decodings,10)
  expect_type(decodings,type="list")
})

test_that("descriptions", {
  bert_modeling$set_model_description(
    eng = "Description",
    native = "Beschreibung",
    abstract_eng = "Abstract",
    abstract_native = "Zusammenfassung",
    keywords_eng = c("Test","Neural Net"),
    keywords_native = c("Test","Neuronales Netz")
  )
  desc<-bert_modeling$get_model_description()
  expect_equal(
    object=desc$eng,
    expected="Description"
  )
  expect_equal(
    object=desc$native,
    expected="Beschreibung"
  )
  expect_equal(
    object=desc$abstract_eng,
    expected="Abstract"
  )
  expect_equal(
    object=desc$abstract_native,
    expected="Zusammenfassung"
  )
  expect_equal(
    object=desc$keywords_eng,
    expected=c("Test","Neural Net")
  )
  expect_equal(
    object=desc$keywords_native,
    expected=c("Test","Neuronales Netz")
  )
})

test_that("software_license", {
  bert_modeling$set_software_license("test_license")
  expect_equal(
    object=bert_modeling$get_software_license(),
    expected=c("test_license")
  )
})

test_that("documentation_license", {
  bert_modeling$set_documentation_license("test_license")
  expect_equal(
    object=bert_modeling$get_documentation_license(),
    expected=c("test_license")
  )
})

test_that("publication_info",{
  bert_modeling$set_publication_info(
    type="developer",
    authors = personList(
      person(given="Max",family="Mustermann")
    ),
    citation="Test Classifier",
    url="https://Test.html"
  )

  bert_modeling$set_publication_info(
    type="modifier",
    authors = personList(
      person(given="Nico",family="Meyer")
    ),
    citation="Test Classifier Revisited",
    url="https://Test_revisited.html"
  )


  pub_info=bert_modeling$get_publication_info()

  expect_equal(
    object=pub_info$developed_by$authors,
    expected=personList(
      person(given="Max",family="Mustermann")
    )
  )
  expect_equal(
    object=pub_info$developed_by$citation,
    expected="Test Classifier"
  )
  expect_equal(
    object=pub_info$developed_by$url,
    expected="https://Test.html"
  )

  expect_equal(
    object=pub_info$modified_by$authors,
    expected=personList(
      person(given="Nico",family="Meyer")
    )
  )
  expect_equal(
    object=pub_info$modified_by$citation,
    expected="Test Classifier Revisited"
  )
  expect_equal(
    object=pub_info$modified_by$url,
    expected="https://Test_revisited.html"
  )
})


test_that("BERT Save Total Model H5", {
  expect_no_error(
    save_ai_model(model=bert_modeling,
                  model_dir = testthat::test_path("tmp_full_models"),
                  save_format = "H5")
  )
})

test_that("BERT Load Total Model H5", {
  bert_modeling<-NULL
  bert_modeling<-load_ai_model(
    model_dir = testthat::test_path(paste0("tmp_full_models/",model_name))
  )
  expect_s3_class(bert_modeling,
                  class="TextEmbeddingModel")
})

test_that("BERT Save Total Model TF with ID", {
  expect_no_error(
    save_ai_model(model=bert_modeling,
                  model_dir = testthat::test_path("tmp_full_models"),
                  save_format = "tf")
  )
})

test_that("BERT Load Total Model TF with ID", {
  bert_modeling<-NULL
  bert_modeling<-load_ai_model(
    model_dir = testthat::test_path(paste0("tmp_full_models/",model_name))
  )
  expect_s3_class(bert_modeling,
                  class="TextEmbeddingModel")
})

test_that("BERT Save Total Model TF without ID", {
  expect_no_error(
    save_ai_model(model=bert_modeling,
                  model_dir = testthat::test_path("tmp_full_models"),
                  save_format = "tf",
                  append_ID = FALSE)
  )
})

test_that("BERT Load Total Model TF without ID", {
  bert_modeling<-NULL
  bert_modeling<-load_ai_model(
    model_dir = testthat::test_path(paste0("tmp_full_models/",model_name_root))
  )
  expect_s3_class(bert_modeling,
                  class="TextEmbeddingModel")
})

test_that("Sustainability Data Loaded", {
  bert_modeling<-NULL
  bert_modeling<-load_ai_model(
    model_dir = testthat::test_path(paste0("tmp_full_models/",model_name_root))
  )
  sustain_data<-bert_modeling$get_sustainability_data()
  expect_equal(nrow(sustain_data),2)
})
