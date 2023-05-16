#'@title Question Answer Models of Type Extraction
#'@description This \link[R6]{R6} class stores the information for modeling a
#'question answer model. This kind of model extracts the answer from a given text.
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
    #'@field model ('transformers.TFAutoModelForQuestionAnswering')\cr
    #'Object of class transformers.TFAutoModelForQuestionAnswering from transformers
    #'python library. Stores the qa model.
    model=NULL,
    #'@field tokenizer ('transformers.AutoTokenizer')\cr
    #'Object of class transformers.AutoTokenizer from transformers python library.
    #'Stores the tokenizer.
    tokenizer=NULL,
    #'@field qa_pipline ('transformers.QuestionAnsweringPipeline')\cr
    #'Object of class transformers.QuestionAnsweringPipeline from transformers
    #'python library.
    qa_pipline=NULL,
    #--------------------------------------------------------------------------
    #'@description Method for creating a new question answer model based on a pretrained
    #'model.
    #'@param model_name \code{Character} Name of the new model.
    #'@param model_version \code{Character} Version of the model.
    #'@param model_language \code{Character} Language the model does support.
    #'@param model_license \code{Character} License of the model.
    #'@param model_dir_path \code{string} Path to the directory where the model is
    #'stored.
    initialize=function(model_name,
                        model_version,
                        model_language,
                        model_license,
                        model_dir_path){
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
    #---------------------------------------------------------------------------
    #'@description Method for saving a question answer model.
    #'@param model_dir_path \code{string} Path to the directory where the model
    #'and the tokenizer should be stored.
    save_model=function(model_dir_path){
        self$model$save_pretrained(save_directory=model_dir_path)
        print(paste(date(),"QA Model Saved."))

        self$tokenizer$save_pretrained(model_dir_path)
        print(paste(date(),"Tokenizer saved."))
    },
    #---------------------------------------------------------------------------
    #'@description Method for loading a question answer model.
    #'@param model_dir_path \code{string} Path to the directory where the model
    #'and the tokenizer are saved.
    load_model=function(model_dir_path){
        transformer = reticulate::import('transformers')
        self$tokenizer<-transformer$AutoTokenizer$from_pretrained(model_dir_path)
        self$model<-transformer$TFAutoModelForQuestionAnswering$from_pretrained(model_dir_path)
        self$qa_pipline<-transformer$pipeline("question-answering",
                                              model=self$model,
                                              tokenizer = self$tokenizer)
    },
    #---------------------------------------------------------------------------
    #'@description Method for extracting answers from a given text.
    #'@param question \code{string} Question to be answered.
    #'@param knowledge_base \code{list} of raw texts where to search for the answer.
    #'@param n_answers \code{int} Number of possible answers generated from the texts.
    #'@param doc_stride \code{int} In the case the knowledge base is to long to
    #'for the model to process at once the text is divided into several overlapping
    #'chunks. This parameter determines the size of the overlap.
    #'@param max_answer_len \code{int} Maximum length in token for possible answers.
    #'Only answers which are shorter are considered for an answer.
    #'@param max_seq_len \code{int} Maximum length of question and knowledge base after
    #'tokenization. The context may be divided into several overlapping chunks.
    #'@param  max_question_len \code{int} The maximum length of the question after tokenization.
    #'Longer sequences are truncated.
    #'@param handle_impossible_answer \code{bool} \code{TRUE} if impossible answers
    #'should be accepted.
    #'@param align_to_words \code{bool} If true \code{TRUE} the algorithm tries to align
    #'the answer to real words which increases the quality of the results for space
    #'separated languages.
    #'@importFrom quanteda.textstats textstat_simil
    answer_question=function(question,
                             knowledge_base,
                             n_answers=1,
                             doc_stride=128,
                             max_answer_len=15,
                             max_seq_len=384,
                             max_question_len=64,
                             handle_impossible_answer=FALSE,
                             align_to_words=TRUE){
      knowledge_base["question"]<-list(question)
      knowledge_base<-unlist(knowledge_base,
                             use.names = TRUE)
      knowledge_base_corpus<-quanteda::corpus(knowledge_base)
      knowledge_base_tokens<-quanteda::tokens(knowledge_base_corpus)
      knowledge_base_dfm<-quanteda::dfm(knowledge_base_tokens)
      knowledge_base_dfm<-quanteda::dfm_weight(
        x=knowledge_base_dfm,
        scheme = "prop")
      similarity_to_question<-quanteda.textstats::textstat_simil(
        x=knowledge_base_dfm[1:(nrow(knowledge_base_dfm)-1),],
        y=knowledge_base_dfm["question",],
        margin="documents",
        method="cosine"
      )

      index_max<-which.max(as.vector(similarity_to_question))
      context = knowledge_base[index_max]

      answer<-self$qa_pipline(question=question,
                              context=context,
                              n_answers=as.integer(n_answers),
                              doc_stride=as.integer(doc_stride),
                              max_answer_len=as.integer(max_answer_len),
                              max_seq_len=as.integer(max_seq_len),
                              max_question_len=as.integer(max_question_len),
                              handle_impossible_answer=FALSE,
                              align_to_words=TRUE)
      return(answer$answer)
    }
  )
)
