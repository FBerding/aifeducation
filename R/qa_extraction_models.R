#' @title Question Extract Answer Models
#'
#'@export
QAExtractModel<-R6::R6Class(
  classname = "QAExtractModel",
  private = list(
    model_info=list(
      model_license=NA,
      model_name=NA,
      model_date=NA,
      model_version=NA,
      model_language=NA
    ),
    publication_info=list(
      developed_by=list(
        autors=NULL,
        citation=NULL,
        url=NULL
      ),
      trained_by=list(
        autors=NULL,
        citation=NULL,
        url=NULL
      ),
      modifided_by=list(
        autors=NULL,
        citation=NULL,
        url=NULL
      )
    ),
    model_description=list(
      eng=NULL,
      native=NULL
    )
  ),
  public = list(
    model=NULL,
    tokenizer=NULL,
    qa_pipline=NULL,
    initialize=function(model_name,
                        model_version,
                        model_language,
                        model_license,
                        model_dir_path){
      if(reticulate::py_module_available("transformers")==FALSE){
        reticulate::py_install('transformers', pip = TRUE)
      }
      transformer = reticulate::import('transformers')
      self$model<-transformer$TFAutoModelForQuestionAnswering$from_pretrained(model_dir_path)
      self$tokenizer<-transformer$AutoTokenizer$from_pretrained(model_dir_path)
      self$qa_pipline<-transformer$pipeline("question-answering",
                                            model=self$model,
                                            tokenizer = self$tokenizer)

      private$model_info$model_name=model_name
      private$model_info$model_version=model_version
      private$model_info$model_language=model_language
      private$model_info$model_date=date()
      private$model_info$model_license=model_license
    },
    save_qa_model=function(model_dir_path){
        self$model$save_pretrained(save_directory=model_dir_path)
        print(paste(date(),"QA Model Saved."))

        self$tokenizer$save_pretrained(model_dir_path)
        print(paste(date(),"Tokenizer saved."))
    },
    load_weights=function(model_dir_path){
        transformer = reticulate::import('transformers')
        self$tokenizer<-transformer$AutoTokenizer$from_pretrained(model_dir_path)
        self$model<-transformer$TFAutoModelForQuestionAnswering$from_pretrained(model_dir_path)
        self$qa_pipline<-transformer$pipeline("question-answering",
                                              model=self$model,
                                              tokenizer = self$tokenizer)
    },
    answer_question=function(question,knowledge_base){
      knowledge_base["question"]<-list(question)
      knowledge_base<-unlist(knowledge_base,
                             use.names = TRUE)
      knowledge_base_corpus<-quanteda::corpus(knowledge_base)
      knowledge_base_tokens<-tokens(knowledge_base_corpus)
      knowledge_base_dfm<-dfm(knowledge_base_tokens)
      similarity_to_question<-quanteda.textstats::textstat_simil(
        x=knowledge_base_dfm[1:(nrow(knowledge_base_dfm)-1),],
        y=knowledge_base_dfm["question",],
        margin="documents",
        method="cosine"
      )

      index_max<-which.max(as.vector(similarity_to_question))
      context = knowledge_base[index_max]

      answer<-self$qa_pipline(question=question,
                              context=context)
      return(answer$answer)
    }
  )
)
