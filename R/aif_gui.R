#'Aifeducation Studio
#'
#'Functions starts a shiny app that represents the Aifeducation Studio
#'
#'@return This function does nothing return. It is used to start a shiny app.
#'
#'@family Graphical User Interface
#'
#'@import iotarelr
#'@importFrom stringr str_extract_all
#'@importFrom stringr str_split_fixed
#'@importFrom stringr str_to_lower
#'@importFrom utils packageVersion
#'@importFrom utils read.csv2
#'@importFrom utils write.csv2
#'@importFrom utils askYesNo
#'@importFrom utils install.packages
#'@importFrom methods is
#'
#'@export
start_aifeducation_studio<-function(){

  #Checking Requirements-------------------------------------------------------
  #Check necessary R packages
  message("Checking R Packages.")
  r_packages=c(
    "shiny",
    "shinyFiles",
    "shinyWidgets",
    "shinydashboard",
    "shinyjs",
    "ggplot2",
    "readtext",
    "readxl",
    "rlang")

  missing_r_packages=NULL
  for(i in 1:length(r_packages)){
    if(requireNamespace(r_packages[i])==FALSE){
      missing_r_packages=append(x=missing_r_packages,
                                values = r_packages[i] )
    }
  }

  if(length(missing_r_packages)>0){
    install_now<-utils::askYesNo(msg=paste("The following R packages are missing for Aifeducation Studio.",
                              missing_r_packages,
                              "Do you want to install them now?"),
                    default = TRUE,
             prompts = getOption("askYesNo", gettext(c("Yes", "No"))))
    if(install_now==TRUE){
      utils::install.packages(missing_r_packages)
    } else {
      stop("Some necessary R Packages are missing.")
    }
  }

  message("Setting the correct conda environment.")
  if(reticulate::py_available(FALSE)==FALSE){
    message("Python is not initalized.")
    if(reticulate::condaenv_exists("aifeducation")==FALSE){
      stop("Aifeducation studio requires a conda environment 'aifeducation' with
      specific python libraries. Please install this. Please refer to the corresponding
      vignette for more details.")
    } else {
      message("Setting conda environment to 'aifeducation'.")
      reticulate::use_condaenv("aifeducation")
      message("Initializing python.")
      if(reticulate::py_available(TRUE)==FALSE){
        stop("Python cannot be initalized. Please check your installation of python.")
      }
    }
  } else {
    message("Python is initalized. Try to start Aifeducation Studio with
            the current environment.")
  }

  message("Checking machine learning frameworks.")
  available_ml_frameworks=NULL
  if(check_aif_py_modules(trace=FALSE,check="pytorch")==TRUE){
    available_ml_frameworks=append(available_ml_frameworks,values = "pytorch")
  }
  if(check_aif_py_modules(trace=FALSE,check="tensorflow")==TRUE){
    available_ml_frameworks=append(available_ml_frameworks,values = "tensorflow")
  }
  if(is.null(available_ml_frameworks)){
    stop("No available machine learning frameworks found.")
  }

  #Exporting Functions for Python
  #These functions must be available in the global environment
  #if(is.null(py_update_aifeducation_progress_bar_epochs)){
  #  stop("Update function for epochs is not available.")
  #}
  #if(is.null(py_update_aifeducation_progress_bar_steps)){
  #  stop("Update function for steps is not available.")
  #}

  #py_update_aifeducation_progress_bar_epochs<-reticulate::py_func(update_aifeducation_progress_bar_epochs)
  #py_update_aifeducation_progress_bar_steps<-reticulate::py_func(update_aifeducation_progress_bar_steps)

  #Start GUI--------------------------------------------------------------------
  options(shiny.reactlog=TRUE)
  #options(shiny.fullstacktrace = TRUE)

  # Define UI ----
  ui <- shinydashboard::dashboardPage(skin = "blue",
                      shinydashboard::dashboardHeader(title="AI for Education",
                                      shinydashboard::dropdownMenuOutput(outputId = "header_notifications")
                      ),
                      shinydashboard::dashboardSidebar(shinydashboard::sidebarMenuOutput(outputId = "sidebar_menu")),
                      shinydashboard::dashboardBody(
                        shinyjs::useShinyjs(),
                        shinydashboard::tabItems(
                          #Configuration: Only shown at the start of the app-------------------------
                          shinydashboard::tabItem(tabName="config",
                                  shiny::fluidPage(
                                    shinydashboard::box(title = "Machine Learning Framework",
                                        solidHeader = TRUE,
                                        status = "primary",

                                        shiny::selectInput(inputId = "config_ml_framework",
                                                    label = "Please choose the ML framework you would like to use:",
                                                    choices = available_ml_frameworks),

                                        shiny::uiOutput(outputId = "config_ml_options")
                                    ),
                                    shinydashboard::box(title = "Sustainability Tracking",
                                        solidHeader = TRUE,
                                        status = "success",

                                        shinyWidgets::materialSwitch(
                                          inputId = "config_track_sustainability",
                                          label = "Activate Sustainability Tracking",
                                          value = TRUE,
                                          right = TRUE,
                                          status = "success"),
                                        shiny::selectInput(
                                          inputId = "config_sustainability_country",
                                          label = "Please choose your country",
                                          choices=country_alpha_3_list,
                                          #choices=NULL,
                                          selected="DEU")
                                    ),
                                    shinydashboard::box(title="Start Session",
                                        width = 12,
                                        solidHeader = TRUE,
                                        status = "primary",
                                        shiny::actionButton(
                                          inputId = "config_start_session",
                                          label = "start session",
                                          width = "100%",
                                          icon = shiny::icon("paper-plane")
                                        )
                                    )
                                  )
                          ),
                          #Start Page after Configuration--------------------------------------------
                          shinydashboard::tabItem(tabName = "start_page",
                                  shiny::fluidPage(
                                    shiny::uiOutput(outputId = "ui_home"))
                          ),
                          #Page Data Preparation-----------------------------------------------------
                          shinydashboard::tabItem(tabName = "data_preparation",
                                  shiny::fluidPage(
                                    shinydashboard::box(title = "Text Sources",
                                        solidHeader = TRUE,
                                        status = "primary",
                                        width = 4,
                                        shinyFiles::shinyDirButton(id="dp_source_dir_select",
                                                       label="Choose Folder",
                                                       title = "Please choose a folder",
                                                       icon=shiny::icon("folder-open")),
                                        shiny::textInput(inputId = "dp_text_source_dir",
                                                  label = shiny::tags$p(shiny::icon("folder"),"Path to Folder")),
                                        shinyWidgets::materialSwitch(inputId = "dp_include_subdirectories",
                                                       label = shiny::tags$p("Include Sub-Folders",shiny::icon("folder-tree")),
                                                       right = TRUE,
                                                       status = "primary")
                                    ),
                                    shinydashboard::box(title="File Types",
                                        solidHeader = TRUE,
                                        status = "primary",
                                        width = 4,
                                        shinyWidgets::materialSwitch(inputId = "dp_include_csv",
                                                       label = shiny::tags$p("Include .csv ",shiny::icon(name = "file-csv")),
                                                       right = TRUE,
                                                       inline = FALSE,
                                                       value = TRUE,
                                                       status = "primary"),
                                        shinyWidgets::materialSwitch(inputId = "dp_include_pdf",
                                                       label = shiny::tags$p("Include .pdf",shiny::icon(name = "file-pdf")),
                                                       right = TRUE,
                                                       inline = FALSE,
                                                       value = TRUE,
                                                       status = "primary"),
                                        shinyWidgets::materialSwitch(inputId = "dp_include_xlsx",
                                                       label = shiny::tags$p("Include .xlsx", shiny::icon(name = "file-excel")),
                                                       right = TRUE,
                                                       inline = FALSE,
                                                       value = TRUE,
                                                       status = "primary"),
                                        shiny::textInput(inputId = "dp_excel_id_column",
                                                  label = "Name of ID column for xlsx files"),
                                        shiny::textInput(inputId = "dp_excel_text_column",
                                                  label = "Name of text column xlsx files")
                                    ),
                                    shinydashboard::box(title = "Text Output",
                                        solidHeader = TRUE,
                                        status = "primary",
                                        width = 4,
                                        shinyFiles::shinyDirButton(id="dp_output_dir_select",
                                                       label = "Select Folder",
                                                       title = "Please select a folder",
                                                       icon=shiny::icon("folder-open")),
                                        shiny::textInput(inputId = "dp_text_output_dir",
                                                  label = shiny::tags$p(shiny::icon("folder"),"Path to Folder")),
                                        shiny::textInput(inputId = "dp_text_output_filename",
                                                  label = shiny::tags$p(shiny::icon("file"),"File Name"))
                                    ),
                                    shinydashboard::box(title = "Start Process",
                                        solidHeader = TRUE,
                                        status = "success",
                                        width = 12,
                                        shiny::actionButton(inputId = "dp_start",
                                                     label = "Create text corpus",
                                                     icon = shiny::icon("paper-plane")))
                                  )
                          ),
                          #Page Create Language Model------------------------------------------------
                          shinydashboard::tabItem(tabName = "create_language_model",
                                  shiny::fluidPage(
                                    shinydashboard::box(title = "Model Architecture",
                                        solidHeader = TRUE,
                                        status = "primary",
                                        width = 4,
                                        shiny::selectInput(inputId = "lm_base_architecture",
                                                    label="Base Architecture",
                                                    choices = c("bert",
                                                                "roberta",
                                                                "deberta_v2",
                                                                "funnel",
                                                                "longformer")),
                                        shiny::uiOutput(outputId = "lm_base_configuration")
                                    ),
                                    shinydashboard::box(title = "Vocabulary",
                                        width = 4,
                                        solidHeader = TRUE,
                                        status = "primary",
                                        shiny::tags$p("Please select a dataset with raw texts for generating a
                           vocabulary for the model."),
                                        shinyFiles::shinyFilesButton(id="lm_text_file_vocab",
                                                         label = "Choose Dataset",
                                                         title = "Please choose a file",
                                                         icon=shiny::icon("file"),
                                                         multiple = FALSE,
                                                         filetype=list(rdata=c('rda','rdata'))),
                                        shiny::textInput(inputId = "lm_vocab_texts_file_path",
                                                  label = shiny::tags$p(shiny::icon("file"),"File path")),
                                        shiny::uiOutput(outputId="lm_vocab_configuration")
                                    ),
                                    shinydashboard::box(title = "Creation",
                                        width = 4,
                                        solidHeader = TRUE,
                                        status = "success",
                                        shiny::tags$p("Please select a directory where the model should be stored."),
                                        shinyFiles::shinyDirButton(id="lm_save_created_model_dir",
                                                       label = "Choose a Folder",
                                                       title = "Please choose a directory",
                                                       icon=shiny::icon("folder-open")),
                                        shiny::textInput(inputId = "lm_save_created_model_dir_path",
                                                  label = shiny::tags$p(shiny::icon("folder"),"Path to Folder")),
                                        shiny::actionButton(inputId = "lm_create",
                                                     label = "Start Creation",
                                                     icon = shiny::icon("paper-plane"))
                                    )
                                  )
                          ),
                          #Page Train Language Model------------------------------------------------
                          shinydashboard::tabItem(tabName = "train_tune_language_model",
                                  shiny::fluidPage(
                                    shinydashboard::box(title = "Base Model",
                                        width = 12,
                                        solidHeader = TRUE,
                                        status = "primary",
                                        shiny::tags$p("Base models consists of several files which are stored in
                                               a folder. Please select the folder that contains the entire model."),
                                        shinyFiles::shinyDirButton(id="lm_db_select_model_for_training",
                                                       label = "Select a Folder",
                                                       title = "Please choose a folder",
                                                       icon=shiny::icon("folder-open")),
                                        shiny::tags$p(shiny::icon("folder"),shiny::tags$b("Path to Base Model:")),
                                        shiny::textOutput(outputId = "lm_db_select_model_for_training_path")
                                    ),
                                    shiny::uiOutput(outputId = "lm_train_raw_texts"),
                                    shiny::uiOutput(outputId = "lm_train_tune_train_settings"),
                                    shiny::uiOutput(outputId = "lm_train_tune_save_settings")
                                  )
                          ),
                          #Page Interface Language Model----------------------------------------------
                          shinydashboard::tabItem(tabName = "interface_language_model",
                                  shiny::fluidPage(
                                    shinydashboard::box(title = "Base Model",
                                        width = 12,
                                        solidHeader = TRUE,
                                        status = "primary",
                                        shiny::tags$p("Please select a base model which should
                             form the basis of your interface."),
                                        shinyFiles::shinyDirButton(id="lm_db_select_model_for_interface",
                                                       label = "Choose a Folder",
                                                       title = "Please choose a folder",
                                                       icon=shiny::icon("folder-open")),
                                        shiny::tags$p(shiny::icon("folder"),shiny::tags$b("Path to Folder with Base Model:")),
                                        shiny::textOutput(outputId = "lm_db_select_model_for_interface_path")
                                    ),
                                    shiny::uiOutput(outputId = "lm_interface_setting")
                                  )
                          ),
                          #Page Use Language Model------------------------------------------------
                          shinydashboard::tabItem(tabName = "use_language_model",
                                  shiny::fluidPage(
                                    shinydashboard::box(title = "Use Language Model",
                                        width = 12,
                                        solidHeader = TRUE,
                                        status = "primary",
                                        shiny::tags$p("Text Embedding Models consist of several files. Please
                                               select the folder that contains the entire model."),
                                        shinyFiles::shinyDirButton(id="lm_db_select_model_for_use",
                                                       label = "Choose a Folder",
                                                       title = "Please choose a folder",
                                                       icon=shiny::icon("folder-open")),
                                        shiny::tags$p(shiny::tags$b("Selected Interface for Masked Language Model:")),
                                        shiny::textOutput(outputId = "lm_use_selected_model_label")
                                    ),
                                    shiny::uiOutput(outputId = "lm_use_tabs")
                                  )
                          ),
                          #Document Page-------------------------------------------------------------
                          shinydashboard::tabItem(tabName = "document_language_model",
                                  shiny::fluidPage(
                                    shinydashboard::box(title = "Document Language Model",
                                        width = 12,
                                        solidHeader = TRUE,
                                        status = "primary",
                                        shiny::tags$p("Text Embedding Models consist of several files. Please
                                               select the folder that contains the entire model."),
                                        shinyFiles::shinyDirButton(id="lm_db_select_model_for_documentation",
                                                       label = "Choose a Folder",
                                                       title = "Please choose a folder",
                                                       icon=shiny::icon("folder-open")),
                                        shiny::tags$p(shiny::tags$b("Selected Interface:")),
                                        shiny::textOutput(outputId = "lm_document_selected_model_label")
                                    ),
                                    shiny::uiOutput(outputId = "lm_document_tabs")
                                  )
                          ),
                          #TextEmbeddingClassifiers Pages--------------------------------------------
                          #Create and Train Page-----------------------------------------------------
                          shinydashboard::tabItem(tabName = "create_and_train_classifier",
                                  shiny::fluidPage(
                                    shiny::fluidRow(
                                      shinydashboard::box(title = "Input Data",
                                          width = 12,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          collapsible = TRUE,
                                          collapsed = FALSE,
                                          shiny::tags$p("Please select the file containing the text embeddings
                             which should be used for training."),
                                          shinyFiles::shinyFilesButton(id="tec_select_embeddings_for_training",
                                                           label = "Select File",
                                                           title = "Please select a file",
                                                           icon=shiny::icon("file"),
                                                           multiple = FALSE,
                                                           filetype=c("rda","rdata")),
                                          shiny::uiOutput(outputId = "tec_embeddings_for_training_overview")
                                      ),
                                      shinydashboard::box(title = "Target Data",
                                          width = 12,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          collapsible = TRUE,
                                          collapsed = FALSE,
                                          shiny::tags$p("Please select the file containing the corresponding
                             classes/categories for each document."),
                                          shiny::tags$p("Possible file formats are .csv, .xlsx, .rda, .rdata."),
                                          shiny::tags$p("Please not that the file must contain a column 'id' which
                             stores the corresponding documents ids."),
                                          shinyFiles::shinyFilesButton(id="tec_select_target_data_for_training",
                                                           label = "Select File",
                                                           title = "please select a file",
                                                           icon=shiny::icon("file"),
                                                           multiple = FALSE,
                                                           filetype=c("csv","xlsx","rda","rdata")),
                                          shiny::uiOutput(outputId = "tec_target_data_for_training_overview")
                                      )
                                    ),
                                    shiny::fluidRow(
                                      shinydashboard::box(title = "Architecture",
                                          width = 12,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          shinydashboard::box(title = "General",
                                              width = 12,
                                              solidHeader = FALSE,
                                              status = "primary",
                                              collapsible = TRUE,
                                              collapsed = FALSE,
                                              shiny::textInput(inputId = "tec_name",
                                                        label = "Model Name",
                                                        width = "100%"),
                                              shiny::textInput(inputId = "tec_label",
                                                        label = "Model Label",
                                                        width = "100%")
                                          ),
                                          shinydashboard::box(title = "Positional Embedding",
                                              width = 12,
                                              solidHeader = FALSE,
                                              status = "primary",
                                              collapsible = TRUE,
                                              collapsed = TRUE,
                                              shinyWidgets::materialSwitch(inputId="tec_add_pos_embedding",
                                                             label="Add Positional Embedding",
                                                             value = TRUE,
                                                             status = "primary")
                                          ),
                                          shinydashboard::box(title = "Encoder Layers",
                                              width = 12,
                                              solidHeader = FALSE,
                                              status = "primary",
                                              collapsible = TRUE,
                                              collapsed = TRUE,
                                              shiny::selectInput(inputId = "tec_attention_type",
                                                          choices = c("fourier","multihead"),
                                                          label = "Attention Type"),
                                              shiny::uiOutput(outputId = "tec_attention_layers_for_training"),
                                              shiny::sliderInput(inputId = "tec_intermediate_size",
                                                          label = "Intermediate Size",
                                                          min = 0,
                                                          value = 512,
                                                          max=8096,
                                                          step = 1,
                                                          round = TRUE),
                                              shiny::sliderInput(inputId = "tec_repeat_encoder",
                                                          label = "Number Encoding Layers",
                                                          value = 1,
                                                          min = 0,
                                                          max=48,
                                                          step = 1,
                                                          round = TRUE),
                                              shiny::sliderInput(inputId="tec_encoder_dropout",
                                                          label="Encoder Layers Dropout",
                                                          value=0.1,
                                                          min=0,
                                                          max=0.99,
                                                          step = 0.01)
                                          ),
                                          shinydashboard::box(title = "Reccurent Layers",
                                              width = 12,
                                              solidHeader = FALSE,
                                              status = "primary",
                                              collapsible = TRUE,
                                              collapsed = TRUE,
                                              shiny::textInput(inputId = "tec_rec",
                                                        label = "Reccurrent Layers"),
                                              shiny::textOutput(outputId = "tec_rec_layer_check"),
                                              shiny::sliderInput(inputId="tec_rec_dropout",
                                                          label="Reccurent Layers Dropout",
                                                          value=0.1,
                                                          min=0,
                                                          max=0.99,
                                                          step = 0.01)
                                          ),
                                          shinydashboard::box(title = "Dense Layers",
                                              width = 12,
                                              solidHeader = FALSE,
                                              status = "primary",
                                              collapsible = TRUE,
                                              collapsed = TRUE,
                                              shiny::textInput(inputId = "tec_hidden",
                                                        label = "Dense Layers",
                                                        width = "100%"),
                                              shiny::textOutput(outputId = "tec_dense_layer_check"),
                                              shiny::sliderInput(inputId="tec_dense_dropout",
                                                          label="Dense Dropout",
                                                          value=0.4,
                                                          min=0,
                                                          max=0.99,
                                                          step = 0.01)
                                          ),
                                          shinydashboard::box(title = "Optimizer",
                                              width = 12,
                                              solidHeader = FALSE,
                                              status = "primary",
                                              collapsible = TRUE,
                                              collapsed = TRUE,
                                              shiny::selectInput(inputId = "tec_optimizer",
                                                          label = "Optimizer",
                                                          choices = c("adam","rmsprop"))
                                          )
                                      ),
                                      shinydashboard::box(title = "Training Settings",
                                          width = 12,
                                          solidHeader = TRUE,
                                          status = "primary",
                                          shinydashboard::box(title = "General Settings",
                                              width = 12,
                                              solidHeader = FALSE,
                                              status = "primary",
                                              collapsible = TRUE,
                                              collapsed = TRUE,
                                              shinyWidgets::materialSwitch(inputId = "tec_balance_class_weights",
                                                             label = "Balance Class Weights",
                                                             value = TRUE,
                                                             status = "primary"),
                                              shiny::sliderInput(inputId = "tec_data_n_test_samples",
                                                          label = "Number of Folds",
                                                          value = 5,
                                                          min=1,
                                                          max=25,
                                                          round = TRUE,
                                                          step = 1),
                                              shiny::sliderInput(inputId = "tec_bsl_val_size",
                                                          label = "Proportion for Validation Sample",
                                                          min=0.02,
                                                          value = 0.25,
                                                          max=0.5,
                                                          step = 0.01),
                                              shiny::numericInput(inputId = "tec_epochs",
                                                           label = "Epochs",
                                                           min = 1,
                                                           value = 40,
                                                           step = 1),
                                              shiny::sliderInput(inputId = "tec_batch_size",
                                                          label = "Batch Size",
                                                          min = 1,
                                                          max= 256,
                                                          value = 32,
                                                          step = 1)
                                          ),
                                          shinydashboard::box(title = "Baseline Model",
                                              width = 12,
                                              solidHeader = FALSE,
                                              status = "primary",
                                              collapsible = TRUE,
                                              collapsed = TRUE,
                                              shinyWidgets::materialSwitch(inputId = "tec_use_baseline",
                                                             value = TRUE,
                                                             label = "Calculate Baseline Model",
                                                             status = "primary")
                                          ),
                                          shinydashboard::box(title = "Balanced Synthetic Cases",
                                              width = 12,
                                              status = "primary",
                                              collapsible = TRUE,
                                              collapsed = TRUE,
                                              shinyWidgets::materialSwitch(inputId = "tec_use_bsc",
                                                             value = FALSE,
                                                             label = "Add Synthetic Cases",
                                                             status = "primary"),
                                              shiny::sliderInput(inputId = "tec_n_cores",
                                                          label = "Number of Cores",
                                                          min = 1,
                                                          max=parallel::detectCores(),
                                                          value = parallel::detectCores()),
                                              shiny::selectInput(inputId = "tec_bsc_methods",
                                                          label = "Method",
                                                          choices = c("dbsmote","adas","smote")),
                                              shiny::sliderInput(inputId = "tec_bsc_max_k",
                                                          label = "Max k",
                                                          value = 10,
                                                          min = 1,
                                                          max = 20,
                                                          step = 1,
                                                          round = TRUE),
                                              shiny::sliderInput(inputId = "tec_bsc_val_size",
                                                          label = "Proportion for Validation Sample",
                                                          min=0.02,
                                                          value = 0.25,
                                                          max=0.5,
                                                          step = 0.01),
                                              shinyWidgets::materialSwitch(inputId = "tec_bsc_add_all",
                                                             label = "Add All Synthetic Cases",
                                                             value = FALSE,
                                                             status = "primary")
                                          ),
                                          shinydashboard::box(
                                            title = "Balanced Pseudo Labeling",
                                            width = 12,
                                            status = "primary",
                                            collapsible = TRUE,
                                            collapsed = TRUE,
                                            shinyWidgets::materialSwitch(inputId = "tec_use_bpl",
                                                           value = FALSE,
                                                           label = "Add Pseudo Labeling",
                                                           status = "primary"),
                                            shiny::sliderInput(inputId = "tec_bpl_max_steps",
                                                        label = "Max Steps",
                                                        value = 5,
                                                        min = 1,
                                                        max = 20,
                                                        step = 1,
                                                        round = TRUE),
                                            shinyWidgets::materialSwitch(inputId = "tec_bpl_model_reset",
                                                           label = "Reset Model After Every Step",
                                                           value = TRUE,
                                                           status = "primary"),
                                            shinyWidgets::materialSwitch(inputId = "tec_bpl_dynamic_inc",
                                                           label = "Dynamic Weight Increase",
                                                           value = FALSE,
                                                           status = "primary"),
                                            shinyWidgets::materialSwitch(inputId = "tec_bpl_balance",
                                                           label = "Balance Pseudo Labels",
                                                           value = FALSE,
                                                           status = "primary"),
                                            shiny::sliderInput(inputId = "tec_bpl_anchor",
                                                        label = "Certainty Anchor",
                                                        value = 1,
                                                        max = 1,
                                                        min = 0,
                                                        step = 0.01),
                                            shiny::uiOutput(outputId = "tec_dynamic_sample_weights"),
                                            shiny::sliderInput(inputId = "tec_bpl_weight_start",
                                                        label = "Start Weights",
                                                        value = 0,
                                                        min = 0,
                                                        max = 2,
                                                        step = 0.01),
                                            shiny::sliderInput(inputId = "tec_bpl_weight_inc",
                                                        label = "Weight Increase Per Step",
                                                        value = 0.02,
                                                        min = 0,
                                                        max = 1,
                                                        step = 0.01)
                                          )
                                      )
                                    ),
                                    shiny::fluidRow(
                                      shinydashboard::box(title="Model Saving",
                                          solidHeader = TRUE,
                                          status = "success",
                                          width = 12,
                                          shinyFiles::shinyDirButton(id="tec_create_select_destination_folder",
                                                         title = "Please select a directory",
                                                         label = "Choose Directory",
                                                         icon=shiny::icon("folder-open")),
                                          shiny::textInput(inputId = "tec_create_select_destination_folder_path",
                                                    label = shiny::tags$p(shiny::icon("folder"),"Directory Path")),
                                          shiny::textInput(inputId = "tec_create_folder_name",
                                                    label = shiny::tags$p(shiny::icon("folder"),"Folder Name")),
                                          shiny::actionButton(inputId="tec_create_test_data_matching",
                                                       label = "Test Data Matching",
                                                       icon = shiny::icon("circle-question")),
                                          shiny::actionButton(inputId = "tec_create_start",
                                                       label = "Start Training",
                                                       icon = shiny::icon("paper-plane"))
                                      )
                                    )
                                  )
                          ),
                          #Classifier Use page--------------------------------------------------------
                          shinydashboard::tabItem(tabName = "use_classifier",
                                  shiny::fluidPage(
                                    shinydashboard::box(title = "Use Classifier",
                                        width = 12,
                                        solidHeader = TRUE,
                                        status = "primary",
                                        shiny::tags$p("A classifier consists of several files. Please select
                                               the folder that contains the entire model."),
                                        shinyFiles::shinyDirButton(id="tec_select_dir_for_use",
                                                       label = "Select Folder",
                                                       title = "Please choose a directory",
                                                       icon=shiny::icon("folder-open")),
                                        shiny::tags$p(shiny::tags$b("Selected Classifier: ")),
                                        shiny::textOutput(outputId = "tec_use_selected_model_label")
                                    ),
                                    shiny::uiOutput(outputId = "tec_use_tabs")
                                  )
                          ),
                          #Document Page-------------------------------------------------------------
                          shinydashboard::tabItem(tabName = "document_classifier",
                                  shiny::fluidPage(
                                    shinydashboard::box(title = "Document Classifier",
                                        width = 12,
                                        solidHeader = TRUE,
                                        status = "primary",
                                        shiny::tags$p("A classifier consists of several files. Please select
                                               the folder that contains the entire model."),
                                        shinyFiles::shinyDirButton(id="tec_db_select_model_for_documentation",
                                                       label = "Choose Folder",
                                                       title = "Please choose a folder",
                                                       icon=shiny::icon("folder-open"))
                                    ),
                                    shiny::uiOutput(outputId = "tec_document_tabs"),
                                    shiny::tags$p(shiny::tags$b("Selected Classifier: ")),
                                    shiny::textOutput(outputId = "tec_document_selected_model_label")
                                  )
                          )
                        )
                      )
  )



  # Define server logic ----
  server <- function(input, output,session) {
    requireNamespace(package="shiny")
    requireNamespace(package="shinydashboard")
    requireNamespace(package="shinyFiles")
    requireNamespace(package="shinyWidgets")
    requireNamespace(package="iotarelr")

    session$onSessionEnded(shiny::stopApp)
    options(shiny.fullstacktrace = FALSE)

    #Logger for progressmodal
    log=shiny::reactiveVal(value = rep(x="",times=15))
    #py_update_aifeducation_progress_bar_epochs<<-reticulate::py_func(update_aifeducation_progress_bar_epochs)
    #py_update_aifeducation_progress_bar_steps<<-reticulate::py_func(update_aifeducation_progress_bar_steps)



    #ReactiveValues--------------------------------------------------------------
    measure_labels=list(
      iota_index="Iota Index",
      min_iota2="Minimum Iota",
      avg_iota2="Average Iota",
      max_iota2="Maximum Iota",
      min_alpha="Minimum Alpha",
      avg_alpha="Average Alpha",
      max_alpha= "Maximum Alpha",
      static_iota_index= "Static Iota Index",
      dynamic_iota_index= "Dynamic Iota Index",
      kalpha_nominal= "Krippendorff's Alpha (Nominal)",
      kalpha_ordinal=   "Krippendorff's Alpha (ordinal)",
      kendall="Kendall's W",
      kappa2_unweighted= "Cohen's Kappa (Unweighted)",
      kappa2_equal_weighted="Weighted Cohen's Kappa (Equal Weights)",
      kappa2_squared_weighted= "Weighted Cohen's Kappa (Squared Weights)",
      kappa_fleiss= "Fleiss' Kappa for Multiple Raters (Without Exact Estimation)",
      percentage_agreement= "Percentage Agreement",
      balanced_accuracy="Average Accuracy within each Class",
      gwet_ac="Gwet's AC1/AC2 Agreement Coefficient"
    )
    measures_scale_level=c(
      "dynamic_iota_index",
      "kalpha_nominal",
      "kalpha_ordinal",
      "kendall",
      "kappa2_unweighted",
      "kappa2_equal_weighted",
      "kappa2_squared_weighted",
      "kappa_fleiss",
      "percentage_agreement",
      "balanced_accuracy",
      "gwet_ac"
    )

    #Starting the app------------------------------------------------------------
    output$sidebar_menu<-shinydashboard::renderMenu({
      shinydashboard::sidebarMenu(id="main_panel",
                  shinydashboard::menuItem(text = "Configuration",
                           tabName = "config")
      )})
    shinydashboard::updateTabItems(selected="config",
                                   inputId = "main_panel")

    output$config_ml_options<-shiny::renderUI({
      if(input$config_ml_framework=="tensorflow"){
        ui<-shiny::tagList(
        shinyWidgets::materialSwitch(
          inputId = "config_tf_cpu_only",
          label = "Tensorflow: Use CPU only",
          value = FALSE,
          right = TRUE,
          status = "primary"),
        shinyWidgets::materialSwitch(
          inputId = "config_tf_gpu_low_memory",
          label = "Tensorflow: Limited GPU memory",
          value = TRUE,
          right = TRUE,
          status = "primary"))
        return(ui)
      } else {
        return(NULL)
      }
    })

    #Finish configuration-------------------------------------------------------
    shiny::observeEvent(input$config_start_session,{

      if(input$config_ml_framework=="tensorflow"){
        if(input$config_tf_cpu_only==TRUE){
          set_config_cpu_only()
        }
        if(input$config_tf_gpu_low_memory==TRUE){
          set_config_gpu_low_memory()
        }
      }

      output$sidebar_menu<-shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(id="main_panel",
                    shinydashboard::menuItem(text="Home",
                             tabName = "start_page",
                             icon = shiny::icon("house")),
                    shinydashboard::menuItem(text = "Data preparation",
                             tabName = "data_preparation",
                             icon = shiny::icon("database")),
                    shinydashboard::menuItem(text = "Language Modeling",
                             icon = shiny::icon("book-open-reader"),
                             shiny::tags$p(shiny::tags$b("Base Models")),
                             shinydashboard::menuSubItem(text = "Create",
                                         tabName = "create_language_model"),
                             shinydashboard::menuSubItem(text="Train/Tune",
                                         tabName = "train_tune_language_model"),
                             shiny::tags$p(shiny::tags$b("Text Embedding Models")),
                             shinydashboard::menuSubItem(text = "Create",
                                         tabName = "interface_language_model"),
                             shinydashboard::menuSubItem(text = "Use",
                                         tabName = "use_language_model"),
                             shinydashboard::menuSubItem(text = "Document",
                                         tabName = "document_language_model")),
                    shinydashboard::menuItem(text = "Classification",
                             icon = shiny::icon("boxes-stacked"),
                             shinydashboard::menuSubItem(text="Create and Train",
                                         tabName = "create_and_train_classifier"),
                             shinydashboard::menuSubItem(text = "Use",
                                         tabName = "use_classifier"),
                             shinydashboard::menuSubItem(text = "Document",
                                         tabName = "document_classifier"))
        )
      })
      shinydashboard::updateTabItems(selected="start_page",
                                     inputId = "main_panel")

    })

    #Header---------------------------------------------------------------------
    chosen_framework<-shiny::eventReactive(input$config_start_session,{

      if(input$config_ml_framework=="tensorflow"){
        if(tf$test$is_built_with_cuda()==TRUE &
           length(tf$config$list_physical_devices('GPU')>0)){
          gpu_available=TRUE
        } else {
          gpu_available=FALSE
        }
      } else if(input$config_ml_framework=="pytorch") {
        if(torch$cuda$is_available()==TRUE){
          gpu_available=TRUE
        } else {
          gpu_available=FALSE
        }
      } else {
        gpu_available=NULL
      }
      return(list(framework=input$config_ml_framework,gpu_acc=gpu_available))
    },ignoreInit = TRUE)

    output$header_notifications<-shinydashboard::renderMenu({
      framework=shinydashboard::notificationItem(text = paste("Active ML Framework: ",chosen_framework()$framework),
                                 shiny::icon("python"))

      gpu_support=shinydashboard::notificationItem(text = paste("GPU Acceleration: ",chosen_framework()$gpu_acc),
                                   shiny::icon("microchip"))

      return(shinydashboard::dropdownMenu(type = "notifications", badgeStatus = "primary",
                          framework,
                          gpu_support))

    })

    #Home Page------------------------------------------------------------------
    output$ui_home<-shiny::renderUI({
      r_packages_names<-c(
        "aifeducation",
        "reticulate",
        "shiny",
        "shinydashboard",
        "shinyFiles",
        "shinyWidgets"
      )
      r_packages_table<-matrix(ncol = 2,
                               nrow = length(r_packages_names))

      for(i in 1:nrow(r_packages_table)){
        r_packages_table[i,1]<-r_packages_names[[i]]
        r_packages_table[i,2]<-as.character(utils::packageVersion(r_packages_names[[i]]))
      }
      colnames(r_packages_table)=c("Package","Version")

      #General py modules
      py_general_table<-matrix(ncol = 3,
                               nrow = 5)
      colnames(py_general_table)<-c("Package","Available","Version")
      py_general_table[1,1]<-"numpy"
      if(reticulate::py_module_available("numpy")==TRUE){
        py_general_table[1,2]<-TRUE
        py_general_table[1,3]<-np["__version__"]
      } else {
        py_general_table[1,2]<-FALSE
        py_general_table[1,3]<-"-"
      }

      py_general_table[2,1]<-"transformers"
      if(reticulate::py_module_available("transformers")==TRUE){
        py_general_table[2,2]<-TRUE
        py_general_table[2,3]<-transformers["__version__"]
      } else {
        py_general_table[2,2]<-FALSE
        py_general_table[2,3]<-"-"
      }

      py_general_table[3,1]<-"tokenizers"
      if(reticulate::py_module_available("tokenizers")==TRUE){
        py_general_table[3,2]<-TRUE
        py_general_table[3,3]<-tok["__version__"]
      } else {
        py_general_table[3,2]<-FALSE
        py_general_table[3,3]<-"-"
      }

      py_general_table[4,1]<-"datasets"
      if(reticulate::py_module_available("datasets")==TRUE){
        py_general_table[4,2]<-TRUE
        py_general_table[4,3]<-datasets["__version__"]
      } else {
        py_general_table[4,2]<-FALSE
        py_general_table[4,3]<-"-"
      }

      py_general_table[5,1]<-"codecarbon"
      if(reticulate::py_module_available("codecarbon")==TRUE){
        py_general_table[5,2]<-TRUE
        py_general_table[5,3]<-codecarbon["__version__"]
      } else {
        py_general_table[5,2]<-FALSE
        py_general_table[5,3]<-"-"
      }

      #Pytorch py modules
      py_torch_table<-matrix(ncol = 3,
                             nrow = 4)
      colnames(py_torch_table)<-c("Package","Available","Version")
      py_torch_table[1,1]<-"torch"
      if(reticulate::py_module_available("torch")==TRUE){
        py_torch_table[1,2]<-TRUE
        py_torch_table[1,3]<-torch["__version__"]
      } else {
        py_torch_table[1,2]<-FALSE
        py_torch_table[1,3]<-"-"
      }

      py_torch_table[2,1]<-"torcheval"
      if(reticulate::py_module_available("torch")==TRUE){
        py_torch_table[2,2]<-TRUE
        py_torch_table[2,3]<-torcheval["__version__"]
      } else {
        py_torch_table[2,2]<-FALSE
        py_torch_table[2,3]<-"-"
      }

      py_torch_table[3,1]<-"safetensors"
      if(reticulate::py_module_available("safetensors")==TRUE){
        py_torch_table[3,2]<-TRUE
        py_torch_table[3,3]<-safetensors["__version__"]
      } else {
        py_torch_table[3,2]<-FALSE
        py_torch_table[3,3]<-"-"
      }

      py_torch_table[4,1]<-"accelerate"
      if(reticulate::py_module_available("accelerate")==TRUE){
        py_torch_table[4,2]<-TRUE
        py_torch_table[4,3]<-accelerate["__version__"]
      } else {
        py_torch_table[4,2]<-FALSE
        py_torch_table[4,3]<-"-"
      }

      #Tensorflow/Keras
      #Pytorch py modules
      py_tf_table<-matrix(ncol = 3,
                          nrow = 2)
      colnames(py_tf_table)<-c("Package","Available","Version")
      py_tf_table[1,1]<-"tensorflow"
      if(reticulate::py_module_available("tensorflow")==TRUE){
        py_tf_table[1,2]<-TRUE
        py_tf_table[1,3]<-tf["__version__"]
      } else {
        py_tf_table[1,2]<-FALSE
        py_tf_table[1,3]<-"-"
      }

      py_tf_table[2,1]<-"keras"
      if(reticulate::py_module_available("keras")==TRUE){
        py_tf_table[2,2]<-TRUE
        py_tf_table[2,3]<-keras["__version__"]
      } else {
        py_tf_table[2,2]<-FALSE
        py_tf_table[2,3]<-"-"
      }

      ui<-list(
        shiny::tags$div(
          shiny::tags$h1("AI for Education"),
          shiny::tags$h2("- Studio -"),
          style="text-align: center;"
        ),
        shinydashboard::box(title="R Packages",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            shiny::renderTable(r_packages_table)),
        shinydashboard::box(title = "Python Packages",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            shiny::renderTable(py_general_table)),
        shinydashboard::box(title="Python Packages - Pytorch",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            shiny::renderTable(py_torch_table)),
        shinydashboard::box(title = "Python Packages - Keras/Tensorflow",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            shiny::renderTable(py_tf_table))
      )
    })

    #Data Preparation Page-------------------------------------------------------
    volumes <- c(Home=fs::path_home(), shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input=input,
                   id="dp_source_dir_select",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = FALSE)
    shinyFiles::shinyDirChoose(input=input,
                   id="dp_output_dir_select",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = FALSE)

    shiny::observeEvent(input$dp_source_dir_select,{
      shiny::updateTextInput(inputId = "dp_text_source_dir",
                      value = shinyFiles::parseDirPath(volumes,input$dp_source_dir_select))
    })
    shiny::observeEvent(input$dp_output_dir_select,{
      shiny::updateTextInput(inputId = "dp_text_output_dir",
                      value = shinyFiles::parseDirPath(volumes,input$dp_output_dir_select))
    })

    #Create Text Data Set--------------------------------------------------------
    shiny::observeEvent(input$dp_start,{
      root_path=input$dp_text_source_dir

      save_file_path=paste0(input$dp_text_output_dir,"/",
                            input$dp_text_output_filename,".rda")

      error_list=NULL
      #Check if all inputs are correctly set
      if(dir.exists(root_path)==FALSE){
        error_list[length(error_list)+1]="Source directory does not exist. Please check
      your directory path."
      }
      if(is.null(input$dp_text_output_dir) | input$dp_text_output_dir==""){
        error_list[length(error_list)+1]="Path to the output directory is missing."
      }
      if(is.null(input$dp_text_output_filename) | input$dp_text_output_filename==""){
        error_list[length(error_list)+1]="File name for the text dataset is missing."
      }
      if(dir.exists(dirname(save_file_path))==FALSE){
        error_list[length(error_list)+1]="Target directory does not exist. Please check
      if a directory exists for saving your data."
      }
      if(input$dp_include_csv==FALSE &
         input$dp_include_pdf==FALSE &
         input$dp_include_csv==FALSE){
        error_list[length(error_list)+1]="No file types selected. Please select
      at least one file type."
      }

      if(length(error_list)>0){
        tmp_ui_error=NULL
        for(i in 1:length(error_list)){
          tmp_ui_error[length(tmp_ui_error)+1]=list(
            shiny::tags$p(error_list[i])
          )
        }

        error_modal<-shiny::modalDialog(
          title = "Error",
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton("Close"),
          shiny::tagList(tmp_ui_error)
        )
        shiny::showModal(error_modal)

      } else {
        shinyWidgets::updateProgressBar(id="pgr_bar_aifeducation",
                          value = 0,
                          total=100)
        shiny::showModal(progress_modal)

        if(input$dp_include_csv==TRUE){
          file_paths_csv=list.files(
            path = root_path,
            include.dirs = FALSE,
            all.files = TRUE,
            full.names = TRUE,
            recursive = input$dp_include_subdirectories,
            pattern = "*.csv")
        } else {
          file_paths_csv=NULL
        }

        if(input$dp_include_pdf==TRUE){
          file_paths_pdf=list.files(
            path = root_path,
            include.dirs = FALSE,
            all.files = TRUE,
            full.names = TRUE,
            recursive = input$dp_include_subdirectories,
            pattern = "*.pdf")
        } else {
          file_paths_pdf=NULL
        }

        if(input$dp_include_xlsx==TRUE){
          file_paths_xslx=list.files(
            path = root_path,
            include.dirs = FALSE,
            all.files = TRUE,
            full.names = TRUE,
            recursive = input$dp_include_subdirectories,
            pattern = "*.xlsx")
        } else {
          file_paths_xlsx=NULL
        }

        all_paths=c(file_paths_pdf,file_paths_csv)

        n_files=length(file_paths_xlsx)+
          length(file_paths_pdf)+
          length(file_paths_csv)

        text_corpus=matrix(nrow = length(all_paths),
                           ncol = 2)
        colnames(text_corpus)=c("id","text")

        counter=1

        shinyWidgets::updateProgressBar(id="pgr_bar_aifeducation",
                          value = counter,
                          total=n_files)

        if(is.null(all_paths)==FALSE){
          for(i in 1:length(all_paths)){
            shinyWidgets::updateProgressBar(id="pgr_bar_aifeducation",
                              value = counter,
                              total=n_files,
                              title = as.character(all_paths[i]))
            tmp_document=readtext::readtext(file=all_paths[i])
            #File name without extension
            #text_corpus[counter,"id"]=stringi::stri_split_fixed(tmp_document$doc_id,pattern=".")[[1]][1]
            text_corpus[counter,"id"]=stringr::str_split_fixed(tmp_document$doc_id,pattern="\\.")[1,-1]
            text_corpus[counter,"text"]=tmp_document$text
            counter=counter+1

            shinyWidgets::updateProgressBar(id="pgr_bar_aifeducation",
                              value = counter,
                              total=n_files,
                              title = as.character(all_paths[i]))
          }
        }


        if(is.null(file_paths_xlsx)==FALSE){
          for(i in 1:length(file_paths_xlsx)){
            shinyWidgets::updateProgressBar(id="pgr_bar_aifeducation",
                              value = counter,
                              total=n_files,
                              title = as.character(file_paths_xlsx[i]))
            tmp_document=readtext::readtext(
              file=file_paths_xlsx[i],
              docid_field = input$dp_excel_id_column,
              text_field = input$dp_excel_text_column)
            tmp_matrix=cbind(tmp_document$doc_id,tmp_document$text)
            colnames(tmp_matrix)=c("id","text")
            text_corpus=rbind(text_corpus,tmp_matrix)
            counter=counter+1

            shinyWidgets::updateProgressBar(id="pgr_bar_aifeducation",
                              value = counter,
                              total=n_files,
                              title = as.character(file_paths_xlsx[i]))
          }
        }

        text_corpus<-as.data.frame(text_corpus)

        shinyWidgets::updateProgressBar(id="pgr_bar_aifeducation",
                          value = counter,
                          total=n_files,
                          title = paste("Write text file to:",save_file_path))

        save(file = save_file_path,text_corpus)

        shiny::removeModal()
        shinyWidgets::show_alert(title = "Success",
                   type="success",
                   text = paste("Created text corpus with",nrow(text_corpus),"documents."))
      }

    })

    #Page Create Language Model-------------------------------------------------
    shinyFiles::shinyFileChoose(input=input,
                    id="lm_text_file_vocab",
                    roots = volumes,
                    filetypes=c("rda","rdata"))
    shiny::observeEvent(input$lm_text_file_vocab,{
      tmp_file_path=shinyFiles::parseFilePaths(volumes,input$lm_text_file_vocab)
      if(nrow(tmp_file_path)>0){
        shiny::updateTextInput(inputId="lm_vocab_texts_file_path",
                        value = tmp_file_path[[1,"datapath"]])
      }
    })

    #Generate UI depending on the central approach
    lm_ui<-shiny::eventReactive(input$lm_base_architecture,{
      #UI for architecture
      if(input$lm_base_architecture=="bert"|
         input$lm_base_architecture=="roberta"|
         input$lm_base_architecture=="deberta_v2"){
        ui_architekture<-shiny::tagList(
          shiny::numericInput(inputId = "lm_max_position_embeddings",
                       label="Maximal sequence length",
                       value=512,
                       min = 100,
                       max=8192,
                       step = 1),
          shiny::numericInput(inputId = "lm_hidden_size",
                       label="Hidden size",
                       value=768,
                       min = 10,
                       max=2048,
                       step = 1),
          shiny::numericInput(inputId = "lm_intermediate_size",
                       label="Intermediate size",
                       value=3072,
                       min = 16,
                       max=16384,
                       step = 1),
          shiny::sliderInput(inputId = "lm_num_hidden_layer",
                      label="n Hidden Layers",
                      value=12,
                      min = 1,
                      max=56,
                      step = 1),
          shiny::sliderInput(inputId = "lm_num_attention_heads",
                      label="n Attentions Heads",
                      value=12,
                      min = 1,
                      max=56,
                      step = 1),
          shiny::selectInput(inputId = "lm_hidden_act",
                      label = "Activation Function",
                      choices = c("gelu", "relu", "silu","gelu_new")),
          shiny::sliderInput(inputId = "lm_hidden_dropout_prob",
                      label="Dropout Probability",
                      value=0.1,
                      min = 0,
                      max=.99,
                      step = .01),
          shiny::sliderInput(inputId = "lm_attention_probs_dropout_prob",
                      label="Attention Dropout Probability",
                      value=0.1,
                      min = 0,
                      max=.99,
                      step = .01)
        )
      } else if(input$lm_base_architecture=="funnel"){
        ui_architekture<-shiny::tagList(
          shiny::numericInput(inputId = "lm_max_position_embeddings",
                       label="Maximal sequence length",
                       value=512,
                       min = 100,
                       max=8192,
                       step = 1),
          shiny::numericInput(inputId = "lm_hidden_size",
                       label="Hidden size",
                       value=768,
                       min = 10,
                       max=2048,
                       step = 1),
          shiny::numericInput(inputId = "lm_target_hidden_size",
                       label="Target Hidden size",
                       value=64,
                       min = 2,
                       max=2048,
                       step = 1),
          shiny::numericInput(inputId = "lm_intermediate_size",
                       label="Intermediate size",
                       value=3072,
                       min = 16,
                       max=16384,
                       step = 1),
          shiny::sliderInput(inputId = "lm_num_blocks",
                      label="n Blocks",
                      value=12,
                      min = 1,
                      max=56,
                      step = 1),
          shiny::sliderInput(inputId = "lm_block_sizes_size",
                      label="Block Sizes",
                      value=4,
                      min = 1,
                      max= 56,
                      step = 1),
          shiny::sliderInput(inputId = "lm_num_decoder_layers",
                      label="n Decoding Layers",
                      value=2,
                      min = 1,
                      max= 24,
                      step = 1),
          shiny::sliderInput(inputId = "lm_num_attention_heads",
                      label="n Attentions Heads",
                      value=12,
                      min = 1,
                      max=56,
                      step = 1),
          shiny::selectInput(inputId = "lm_hidden_act",
                      label = "Activation Function",
                      choices = c("gelu", "relu", "silu","gelu_new")),
          shiny::sliderInput(inputId = "lm_hidden_dropout_prob",
                      label="Dropout Probability",
                      value=0.1,
                      min = 0,
                      max=.99,
                      step = .01),
          shiny::sliderInput(inputId = "lm_attention_probs_dropout_prob",
                      label="Attention Dropout Probability",
                      value=0.1,
                      min = 0,
                      max=.99,
                      step = .01),
          shiny::sliderInput(inputId = "lm_activation_dropout ",
                      label="Activation Dropout Probability",
                      value=0.0,
                      min = 0,
                      max=.99,
                      step = .01)
        )
      } else if(input$lm_base_architecture=="longformer"){
        ui_architekture<-shiny::tagList(
          shiny::numericInput(inputId = "lm_max_position_embeddings",
                       label="Maximal sequence length",
                       value=512,
                       min = 100,
                       max=8192,
                       step = 1),
          shiny::numericInput(inputId = "lm_hidden_size",
                       label="Hidden size",
                       value=768,
                       min = 10,
                       max=2048,
                       step = 1),
          shiny::numericInput(inputId = "lm_intermediate_size",
                       label="Intermediate size",
                       value=3072,
                       min = 16,
                       max=16384,
                       step = 1),
          shiny::sliderInput(inputId = "lm_num_hidden_layer",
                      label="n Hidden Layers",
                      value=12,
                      min = 1,
                      max=56,
                      step = 1),
          shiny::sliderInput(inputId = "lm_num_attention_heads",
                      label="n Attentions Heads",
                      value=12,
                      min = 1,
                      max=56,
                      step = 1),
          shiny::numericInput(inputId = "lm_attention_window ",
                       label="Size Attention Window",
                       value=512,
                       min = 10,
                       max=8192,
                       step = 1),
          shiny::selectInput(inputId = "lm_hidden_act",
                      label = "Activation Function",
                      choices = c("gelu", "relu", "silu","gelu_new")),
          shiny::sliderInput(inputId = "lm_hidden_dropout_prob",
                      label="Dropout Probability",
                      value=0.1,
                      min = 0,
                      max=.99,
                      step = .01),
          shiny::sliderInput(inputId = "lm_attention_probs_dropout_prob",
                      label="Attention Dropout Probability",
                      value=0.1,
                      min = 0,
                      max=.99,
                      step = .01)
        )
      }

      #UI for vocab
      if(input$lm_base_architecture=="bert"|
         input$lm_base_architecture=="funnel"){
        ui_vocab<-shiny::tagList(
          shiny::numericInput(inputId = "lm_vocab_size",
                       label="Size of Vocabulary",
                       value=30522,
                       min = 100,
                       max=200000,
                       step = 1),
          shinyWidgets::materialSwitch(inputId = "lm_vocab_do_lower_case",
                         value = FALSE,
                         label = "Transform to Lower Case",
                         status = "primary")
        )
      } else if(input$lm_base_architecture=="roberta"){
        ui_vocab<-shiny::tagList(
          shiny::numericInput(inputId = "lm_vocab_size",
                       label="Size of Vocabulary",
                       value=30522,
                       min = 100,
                       max=200000,
                       step = 1),
          shinyWidgets::materialSwitch(inputId = "lm_add_prefix_space",
                         value = FALSE,
                         right = TRUE,
                         label = "Add Prefix Space",
                         status = "primary"),
          shinyWidgets::materialSwitch(inputId = "lm_trim_offsets",
                         value = TRUE,
                         right = TRUE,
                         label = "Trim Offsets",
                         status = "primary"),
          shinyWidgets::materialSwitch(inputId = "lm_vocab_do_lower_case",
                         value = FALSE,
                         label = "Transform to Lower Case",
                         status = "primary")
        )
      } else if(input$lm_base_architecture=="deberta_v2"|
                input$lm_base_architecture=="longformer"){
        ui_vocab<-shiny::tagList(
          shiny::numericInput(inputId = "lm_vocab_size",
                       label="Size of Vocabulary",
                       value=30522,
                       min = 100,
                       max=200000,
                       step = 1),
          shinyWidgets::materialSwitch(inputId = "lm_add_prefix_space",
                         value = FALSE,
                         right = TRUE,
                         label = "Add Prefix Space",
                         status = "primary"),
          shinyWidgets::materialSwitch(inputId = "lm_trim_offsets",
                         value = TRUE,
                         right = TRUE,
                         label = "Trim Offsets",
                         status = "primary"),
          shinyWidgets::materialSwitch(inputId = "lm_vocab_do_lower_case",
                         value = FALSE,
                         label = "Transform to Lower Case",
                         status = "primary"),
        )
      }

      return(list(
        ui_architekture,
        ui_vocab
      ))
    })

    output$lm_base_configuration<-shiny::renderUI({lm_ui()[1]})
    output$lm_vocab_configuration<-shiny::renderUI({lm_ui()[2]})

    #Save Location shinydashboard::box
    shinyFiles::shinyDirChoose(input=input,
                   id="lm_save_created_model_dir",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = TRUE)

    shiny::observeEvent(input$lm_save_created_model_dir,{
      shiny::updateTextInput(inputId = "lm_save_created_model_dir_path",
                      value = shinyFiles::parseDirPath(volumes,input$lm_save_created_model_dir))
    })

    #Create model
    shiny::observeEvent(input$lm_create,{
      #Check inputs
      error_list=NULL
      if(!dir.exists(input$lm_save_created_model_dir_path)){
        error_list[length(error_list)+1]=list(shiny::tags$p(
          "The destination directory does not exist. Please check the path
        and/or create that directory.")
        )
      }

      if(!file.exists(input$lm_vocab_texts_file_path)){
        error_list[length(error_list)+1]=list(shiny::tags$p(
          "The file containing the raw texts does not exist. Please check
        the corresponding path.")
        )
      } else {
        raw_text_object=load(input$lm_vocab_texts_file_path)
        raw_texts<-get(x=raw_text_object)

        if("text"%in%colnames(raw_texts)==FALSE){
          error_list[length(error_list)+1]=list(shiny::tags$p(
            "The file with the raw texts does not contain a column 'text'.
          Please check the file.")
          )
        }
      }

      if(length(error_list)==0){
        shiny::showModal(progress_modal)

        raw_texts=as.data.frame(raw_texts)
        trace=TRUE

        withCallingHandlers({
          #Clear Log Output
          log(rep(x="",times=15))
          shinyjs::html(id="pgr_text_output_aifeducation",html = "")

          if(input$lm_base_architecture=="bert"){
            create_bert_model(
              ml_framework=input$config_ml_framework,
              model_dir=input$lm_save_created_model_dir_path,
              vocab_raw_texts=raw_texts$text,
              vocab_size=input$lm_vocab_size,
              vocab_do_lower_case=input$lm_vocab_do_lower_case,
              max_position_embeddings=input$lm_max_position_embeddings,
              hidden_size=input$lm_hidden_size,
              num_hidden_layer=input$lm_num_hidden_layer,
              num_attention_heads=input$lm_num_attention_heads,
              intermediate_size=input$lm_intermediate_size,
              hidden_act=input$lm_hidden_act,
              hidden_dropout_prob=input$lm_hidden_dropout_prob,
              attention_probs_dropout_prob=input$lm_attention_probs_dropout_prob,
              sustain_track=input$config_track_sustainability,
              sustain_iso_code=input$config_sustainability_country,
              sustain_region=NULL,
              sustain_interval=15,
              trace=trace)
          } else if(input$lm_base_architecture=="roberta"){
            create_roberta_model(
              ml_framework=input$config_ml_framework,
              model_dir=input$lm_save_created_model_dir_path,
              vocab_raw_texts=raw_texts$text,
              vocab_size=input$lm_vocab_size,
              add_prefix_space=input$lm_add_prefix_space,
              trim_offsets=input$lm_trim_offsets,
              max_position_embeddings=input$lm_max_position_embeddings,
              hidden_size=input$lm_hidden_size,
              num_hidden_layer=input$lm_num_hidden_layer,
              num_attention_heads=input$lm_num_attention_heads,
              intermediate_size=input$lm_intermediate_size,
              hidden_act=input$lm_hidden_act,
              hidden_dropout_prob=input$lm_hidden_dropout_prob,
              attention_probs_dropout_prob=input$lm_attention_probs_dropout_prob,
              sustain_track=input$config_track_sustainability,
              sustain_iso_code=input$config_sustainability_country,
              sustain_region=NULL,
              sustain_interval=15,
              trace=trace)

          } else if(input$lm_base_architecture=="deberta_v2"){
            create_deberta_v2_model(
              ml_framework=input$config_ml_framework,
              model_dir=input$lm_save_created_model_dir_path,
              vocab_raw_texts=raw_texts$text,
              vocab_size=input$lm_vocab_size,
              add_prefix_space=input$lm_add_prefix_space,
              trim_offsets=input$lm_trim_offsets,
              do_lower_case=input$lm_vocab_do_lower_case,
              max_position_embeddings=input$lm_max_position_embeddings,
              hidden_size=input$lm_hidden_size,
              num_hidden_layer=input$lm_num_hidden_layer,
              num_attention_heads=input$lm_num_attention_heads,
              intermediate_size=input$lm_intermediate_size,
              hidden_act=input$lm_hidden_act,
              hidden_dropout_prob=input$lm_hidden_dropout_prob,
              attention_probs_dropout_prob=input$lm_attention_probs_dropout_prob,
              sustain_track=input$config_track_sustainability,
              sustain_iso_code=input$config_sustainability_country,
              sustain_region=NULL,
              sustain_interval=15,
              trace=trace)

          } else if(input$lm_base_architecture=="longformer"){
            create_longformer_model(
              ml_framework=input$config_ml_framework,
              model_dir=input$lm_save_created_model_dir_path,
              vocab_raw_texts=raw_texts$text,
              vocab_size=input$lm_vocab_size,
              add_prefix_space=input$lm_add_prefix_space,
              trim_offsets=input$lm_trim_offsets,
              max_position_embeddings=input$lm_max_position_embeddings,
              hidden_size=input$lm_hidden_size,
              num_hidden_layer=input$lm_num_hidden_layer,
              num_attention_heads=input$lm_num_attention_heads,
              intermediate_size=input$lm_intermediate_size,
              hidden_act=input$lm_hidden_act,
              hidden_dropout_prob=input$lm_hidden_dropout_prob,
              attention_probs_dropout_prob=input$lm_attention_probs_dropout_prob,
              attention_window=input$lm_attention_window,
              sustain_track=input$config_track_sustainability,
              sustain_iso_code=input$config_sustainability_country,
              sustain_region=NULL,
              sustain_interval=15,
              trace=trace)

          } else if(input$lm_base_architecture=="funnel"){
            blocks=rep(x=input$lm_block_sizes_size,times=input$lm_num_blocks)

            create_funnel_model(
              ml_framework=input$config_ml_framework,
              model_dir=input$lm_save_created_model_dir_path,
              vocab_raw_texts=raw_texts$text,
              vocab_size=input$lm_vocab_size,
              vocab_do_lower_case=input$lm_vocab_do_lower_case,
              max_position_embeddings=input$lm_max_position_embeddings,
              hidden_size=input$lm_hidden_size,
              target_hidden_size=input$lm_target_hidden_size,
              block_sizes=blocks,
              num_attention_heads=input$lm_num_attention_heads,
              intermediate_size=input$lm_intermediate_size,
              num_decoder_layers=input$lm_num_decoder_layers,
              pooling_type="mean",
              hidden_act=input$lm_hidden_act,
              hidden_dropout_prob=input$lm_hidden_dropout_prob,
              attention_probs_dropout_prob=input$lm_attention_probs_dropout_prob,
              activation_dropout=input$lm_activation_dropout,
              sustain_track=input$config_track_sustainability,
              sustain_iso_code=input$config_sustainability_country,
              sustain_region=NULL,
              sustain_interval=15,
              trace=trace)
          }
        },
        message=function(m){
          #Vector with old messages
          old_messages<-log()
          new_message=m$message

          #add new message and remove oldes message
          output_print<-c(old_messages[2:length(old_messages)],new_message)
          shinyjs::html(id="pgr_text_output_aifeducation",
                        html = paste(output_print,collapse = "<br>"),
                        add = FALSE)
          #re-assign output
          log(output_print)
        }
        )
        shiny::removeModal()
      } else {
        #Show error messages
        shinyWidgets::show_alert(
          title = "Error",
          text = shiny::tagList(error_list),
          type = "error")
        #error_modal<-shiny::modalDialog(
        #  title = "Error",
        #  size = "l",
        #  easyClose = TRUE,
        #  footer = shiny::modalButton("Close"),
        #  shiny::tagList(error_list)
        #)
        #shiny::showModal(error_modal)
      }
    })

    #Page Train and Tune a Language Model----------------------------------------
    shinyFiles::shinyDirChoose(input=input,
                   id="lm_db_select_model_for_training",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = TRUE)

    model_path_train_LM<-shiny::eventReactive(input$lm_db_select_model_for_training,{
      path=shinyFiles::parseDirPath(volumes,input$lm_db_select_model_for_training)

      output$lm_db_select_model_for_training_path=shiny::renderText(path)
      return(path)
    })

    train_tune_model_architecture<-shiny::eventReactive(model_path_train_LM(),{
      shinyWidgets::show_alert(title="Loading",
                               text = "Please wait",
                               type="info")
      model_path<-model_path_train_LM()
      if(!is.null(model_path)){
        if(file.exists(paste0(model_path,
                              "/",
                              "tf_model.h5"))){
          model<-transformers$TFAutoModel$from_pretrained(model_path)
          model_architecture<-model$config$architectures
          max_position_embeddings=model$config$max_position_embeddings
          model_exists=TRUE
        } else if(file.exists(paste0(model_path,
                                     "/",
                                     "pytorch_model.bin"))){
          model<-transformers$AutoModel$from_pretrained(model_path)
          model_architecture<-model$config$architectures
          max_position_embeddings=model$config$max_position_embeddings
          model_exists=TRUE
        } else {
          model_architecture=NULL
          max_position_embeddings=NULL
          model_exists=FALSE
        }
      } else {
        model_architecture=NULL
        max_position_embeddings=NULL
        model_exists=NULL
      }
      shinyWidgets::closeSweetAlert()
      return(list(model_architecture,max_position_embeddings,model_exists))
    })

    shiny::observe({
      if(!is.null(train_tune_model_architecture()$model_exists)){
        if(train_tune_model_architecture()$model_exists==FALSE){
          shinyWidgets::show_alert(
            title = "Error",
            text = "There is no model to load in the directory.",
            type = "error")
        }
      }
    })

    #Create shinydashboard::box for Raw Text Selection
    output$lm_train_raw_texts<-shiny::renderUI({
      if(!is.null(train_tune_model_architecture()[[2]])){
        final_box<-shinydashboard::box(
          title="Raw Texts",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          shiny::tags$p("Please select the file containing the raw texts for training."),
          shinyFiles::shinyFilesButton(id="lm_db_select_raw_txt_for_training",
                           label="Select File",
                           title="Please select a file",
                           icon=shiny::icon("file"),
                           multiple=FALSE,
                           filetype=c("rda","rdata")),
          shiny::textInput(inputId = "lm_db_select_raw_txt_for_training_path",
                    label = shiny::tags$p(shiny::icon("file"),"File Path"))
        )

        return(final_box)
      } else {
        return(NULL)
      }
    })

    shinyFiles::shinyFileChoose(input=input,
                    id="lm_db_select_raw_txt_for_training",
                    roots = volumes,
                    filetypes=c("rda","rdata"))
    shiny::observeEvent(input$lm_db_select_raw_txt_for_training,{
      tmp_file_path=shinyFiles::parseFilePaths(volumes,input$lm_db_select_raw_txt_for_training)
      if(nrow(tmp_file_path)>0){
        shiny::updateTextInput(inputId="lm_db_select_raw_txt_for_training_path",
                        value = tmp_file_path[[1,"datapath"]])
      }
    })

    #shinydashboard::box for Training Settings
    output$lm_train_tune_train_settings<-shiny::renderUI({
      if(!is.null(train_tune_model_architecture()[[2]])){
        model_architecture=train_tune_model_architecture()[1]
        max_position_embeddings=train_tune_model_architecture()[[2]]
        if(model_architecture=="BertModel"){
          ui_training_setting<-shiny::fluidRow(
            shiny::column(width = 6,
                   shiny::sliderInput(inputId = "lm_chunk_size",
                               label="Chunk Size",
                               value=250,
                               min = 100,
                               max=max_position_embeddings,
                               step = 1),
                   shiny::sliderInput(inputId = "lm_min_seq_len",
                               label="Minimal Sequence Length",
                               value=50,
                               min = 10,
                               max=max_position_embeddings,
                               step = 1),
                   shiny::sliderInput(inputId = "lm_p_mask",
                               label="Probability of Token Masking",
                               value=.15,
                               min = .05,
                               max=.95,
                               step = .01),
                   shiny::sliderInput(inputId = "lm_val_size",
                               label="Validation Size",
                               value=.10,
                               min = .01,
                               max=.99,
                               step = .01),
                   shiny::sliderInput(inputId = "lm_batch_size",
                               label="Batch Size",
                               value=12,
                               min = 1,
                               max=64,
                               step = 1)
            ),
            shiny::column(width = 6,
                   shiny::numericInput(inputId = "lm_n_epoch",
                                label="N Epochs",
                                value=50,
                                min = 1,
                                max=NA,
                                step = 1),
                   shiny::numericInput(inputId = "lm_learning_rate",
                                label="Learning Rate",
                                value=0.003,
                                min = 0.0001,
                                max=1,
                                step = .001),
                   shinyWidgets::materialSwitch(inputId = "lm_full_sequences_only",
                                  value = FALSE,
                                  label = shiny::tags$b("Full Sequences Only"),
                                  status = "primary"),
                   shinyWidgets::materialSwitch(inputId = "lm_whole_word ",
                                  value = TRUE,
                                  label = shiny::tags$b("Whole Word Masking"),
                                  status = "primary")
            )

          )
        } else {
          ui_training_setting<-shiny::fluidRow(
            shiny::column(width = 6,
                   shiny::sliderInput(inputId = "lm_p_mask",
                               label="Probability of Token Masking",
                               value=.15,
                               min = .05,
                               max=.95,
                               step = .01),
                   shiny::sliderInput(inputId = "lm_val_size",
                               label="Validation Size",
                               value=.10,
                               min = .01,
                               max=.99,
                               step = .01),
                   shiny::sliderInput(inputId = "lm_batch_size",
                               label="Batch Size",
                               value=12,
                               min = 1,
                               max=64,
                               step = 1),
                   shiny::sliderInput(inputId = "lm_chunk_size",
                               label="Chunk Size",
                               value=250,
                               min = 100,
                               max=max_position_embeddings,
                               step = 1),
                   shiny::sliderInput(inputId = "lm_min_seq_len",
                               label="Minimal Sequence Length",
                               value=50,
                               min = 10,
                               max=max_position_embeddings,
                               step = 1)
            ),
            shiny::column(width = 6,
                   shiny::numericInput(inputId = "lm_n_epoch",
                                label="N Epochs",
                                value=50,
                                min = 1,
                                max=NA,
                                step = 1),
                   shiny::numericInput(inputId = "lm_learning_rate",
                                label="Learning Rate",
                                value=0.003,
                                min = 0.0001,
                                max=1,
                                step = .001),
                   shinyWidgets::materialSwitch(inputId = "lm_full_sequences_only",
                                  value = FALSE,
                                  label = shiny::tags$b("Full Sequences Only"),
                                  status="primary")
            )
          )
        }

        final_box<-shinydashboard::box(title = "Train and Tune Settings",
                       width = 12,
                       status = "primary",
                       solidHeader = TRUE,
                       ui_training_setting
        )

        return(final_box)
      } else {
        return(NULL)
      }
    })

    #shinydashboard::box for Saving trained/tuned model
    output$lm_train_tune_save_settings<-shiny::renderUI({
      if(!is.null(train_tune_model_architecture()[[2]])){
        ui_start<-shinydashboard::box(
          title = "Start Training/Tuning",
          width = 12,
          solidHeader = TRUE,
          status = "success",
          shiny::tags$p("Please select a folder where the trained/tuned
                     model should be saved"),
          shinyFiles::shinyDirButton(id="lm_db_select_final_model_destination",
                         label = "Choose a Folder",
                         title = "Please choose a Folder",
                         icon=shiny::icon("folder-open")),
          shiny::textInput(inputId =  "lm_db_select_final_model_destination_path",
                    label = shiny::tags$p(shiny::icon("folder"),"Path to Folder")),
          shiny::actionButton(inputId = "lm_train_tune_start",
                       label = "Start Training/Tuning",
                       icon = shiny::icon("paper-plane"))
        )
        return(ui_start)
      } else {
        return(NULL)
      }

    })

    #Final Management for final destination of the model
    shinyFiles::shinyDirChoose(input=input,
                   id="lm_db_select_final_model_destination",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = TRUE)
    shiny::observeEvent(input$lm_db_select_final_model_destination,{
      shiny::updateTextInput(inputId = "lm_db_select_final_model_destination_path",
                      value = shinyFiles::parseDirPath(volumes,input$lm_db_select_final_model_destination))
    })

    #Training and Tuning
    shiny::observeEvent(input$lm_train_tune_start,{
      base_model_path=model_path_train_LM()
      raw_text_path=input$lm_db_select_raw_txt_for_training_path
      destination_dir=input$lm_db_select_final_model_destination_path
      model_architecture=train_tune_model_architecture()[1]

      error_list=NULL
      if(!dir.exists(destination_dir)){
        error_list[length(error_list)+1]=list(shiny::tags$p(
          "The destination directory does not exist. Please check the path
        and/or create that directory.")
        )
      }

      if(is.null(input$lm_db_select_raw_txt_for_training_path)){
        error_list[length(error_list)+1]=list(shiny::tags$p(
          "Path for the file containing the raw texts is missing."))
      } else {
        if(input$lm_db_select_raw_txt_for_training_path==""){
          error_list[length(error_list)+1]=list(shiny::tags$p(
            "Path for the file containing the raw texts is missing."))
        } else {
          if(!file.exists(raw_text_path)){
            error_list[length(error_list)+1]=list(shiny::tags$p(
              "The file containing the raw texts does not exist. Please check
        the corresponding path."))
          } else {
            raw_text_object=load(raw_text_path)
            raw_texts<-get(x=raw_text_object)

            if("text"%in%colnames(raw_texts)==FALSE){
              error_list[length(error_list)+1]=list(shiny::tags$p(
                "The file with the raw texts does not contain a column 'text'.
          Please check the file."))
            }
          }
        }
      }

      if(length(error_list)==0){
        shiny::showModal(progress_modal)

        raw_texts=as.data.frame(raw_texts)
        trace=TRUE

        withCallingHandlers({
          #Clear Log Output
          log(rep(x="",times=15))
          shinyjs::html(id="pgr_text_output_aifeducation",html = "")

          if(input$lm_base_architecture=="bert"){
            train_tune_bert_model(
              ml_framework=input$config_ml_framework,
              output_dir=destination_dir,
              model_dir_path=base_model_path,
              raw_texts=raw_texts$text,
              p_mask=input$lm_p_mask,
              whole_word=TRUE,
              val_size=input$lm_val_size,
              n_epoch=input$lm_n_epoch,
              batch_size=input$lm_batch_size,
              chunk_size=input$lm_chunk_size,
              full_sequences_only=input$lm_full_sequences_only,
              min_seq_len=input$lm_min_seq_len,
              learning_rate=input$lm_learning_rate,
              n_workers=1,
              multi_process=FALSE,
              sustain_track=input$config_track_sustainability,
              sustain_iso_code=input$config_sustainability_country,
              sustain_region=NULL,
              sustain_interval=15,
              trace=trace,
              keras_trace=0,
              pytorch_trace=0)

          } else if(input$lm_base_architecture=="roberta"){
            train_tune_roberta_model(
              ml_framework=input$config_ml_framework,
              output_dir=destination_dir,
              model_dir_path=base_model_path,
              raw_texts=raw_texts$text,
              p_mask=input$lm_p_mask,
              val_size=input$lm_val_size,
              n_epoch=input$lm_n_epoch,
              batch_size=input$lm_batch_size,
              chunk_size=input$lm_chunk_size,
              full_sequences_only=input$lm_full_sequences_only,
              min_seq_len=input$lm_min_seq_len,
              learning_rate=input$lm_learning_rate,
              n_workers=1,
              multi_process=FALSE,
              sustain_track=input$config_track_sustainability,
              sustain_iso_code=input$config_sustainability_country,
              sustain_region=NULL,
              sustain_interval=15,
              trace=trace,
              keras_trace=0,
              pytorch_trace=0)

          } else if(input$lm_base_architecture=="deberta_v2"){
            train_tune_deberta_v2_model(
              ml_framework=input$config_ml_framework,
              output_dir=destination_dir,
              model_dir_path=base_model_path,
              raw_texts=raw_texts$text,
              p_mask=input$lm_p_mask,
              val_size=input$lm_val_size,
              n_epoch=input$lm_n_epoch,
              batch_size=input$lm_batch_size,
              chunk_size=input$lm_chunk_size,
              full_sequences_only=input$lm_full_sequences_only,
              min_seq_len=input$lm_min_seq_len,
              learning_rate=input$lm_learning_rate,
              n_workers=1,
              multi_process=FALSE,
              sustain_track=input$config_track_sustainability,
              sustain_iso_code=input$config_sustainability_country,
              sustain_region=NULL,
              sustain_interval=15,
              trace=trace,
              keras_trace=0,
              pytorch_trace=0)

          } else if(input$lm_base_architecture=="longformer"){
            train_tune_longformer_model(
              ml_framework=input$config_ml_framework,
              output_dir=destination_dir,
              model_dir_path=base_model_path,
              raw_texts=raw_texts$text,
              p_mask=input$lm_p_mask,
              val_size=input$lm_val_size,
              n_epoch=input$lm_n_epoch,
              batch_size=input$lm_batch_size,
              chunk_size=input$lm_chunk_size,
              full_sequences_only=input$lm_full_sequences_only,
              min_seq_len=input$lm_min_seq_len,
              learning_rate=input$lm_learning_rate,
              n_workers=1,
              multi_process=FALSE,
              sustain_track=input$config_track_sustainability,
              sustain_iso_code=input$config_sustainability_country,
              sustain_region=NULL,
              sustain_interval=15,
              trace=trace,
              keras_trace=0,
              pytorch_trace=0)

          } else if(input$lm_base_architecture=="funnel"){
            train_tune_funnel_model(
              ml_framework=input$config_ml_framework,
              output_dir=destination_dir,
              model_dir_path=base_model_path,
              raw_texts=raw_texts$text,
              p_mask=input$lm_p_mask,
              val_size=input$lm_val_size,
              n_epoch=input$lm_n_epoch,
              batch_size=input$lm_batch_size,
              chunk_size=input$lm_chunk_size,
              full_sequences_only=input$lm_full_sequences_only,
              min_seq_len=input$lm_min_seq_len,
              learning_rate=input$lm_learning_rate,
              n_workers=1,
              multi_process=FALSE,
              sustain_track=input$config_track_sustainability,
              sustain_iso_code=input$config_sustainability_country,
              sustain_region=NULL,
              sustain_interval=15,
              trace=trace,
              keras_trace=0,
              pytorch_trace=0)
          }
        },
        message=function(m){
          #Vector with old messages
          old_messages<-log()
          new_message=m$message

          #add new message and remove oldes message
          output_print<-c(old_messages[2:length(old_messages)],new_message)

          shinyjs::html(id="pgr_text_output_aifeducation",
                        html = paste(output_print,collapse = "<br>"),
                        add = FALSE)
          #re-assign output
          log(output_print)
        }
        )
        shiny::removeModal()
      } else {
        #Show error messages
        shinyWidgets::show_alert(title = "Error",
                   text = shiny::tagList(error_list),
                   type = "error")
        #error_modal<-shiny::modalDialog(
        #  title = "Error",
        #  size = "l",
        #  easyClose = TRUE,
        #  footer = shiny::modalButton("Close"),
        #  shiny::tagList(error_list)
        #)
        #shiny::showModal(error_modal)
      }
    })

    #Interface Page-------------------------------------------------------------
    shinyFiles::shinyDirChoose(input=input,
                   id="lm_db_select_model_for_interface",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = TRUE)

    model_path_interface_LM<-shiny::eventReactive(input$lm_db_select_model_for_interface,{
      path=shinyFiles::parseDirPath(volumes,input$lm_db_select_model_for_interface)
      output$lm_db_select_model_for_interface_path<-shiny::renderText({path})
      return(path)
    })

    interface_architecture<-shiny::eventReactive(model_path_interface_LM(),{
      model_path<-model_path_interface_LM()
      if(file.exists(paste0(model_path,
                            "/",
                            "tf_model.h5"))){
        model<-transformers$TFAutoModel$from_pretrained(model_path)
        model_architecture<-model$config$architectures
        max_position_embeddings=model$config$max_position_embeddings
      } else if(file.exists(paste0(model_path,
                                   "/",
                                   "pytorch_model.bin"))){
        model<-transformers$AutoModel$from_pretrained(model_path)
        model_architecture<-model$config$architectures
        max_position_embeddings=model$config$max_position_embeddings
      } else {
        model_architecture=NULL
        max_position_embeddings=NULL
      }
      return(list(model_architecture,max_position_embeddings))
    })

    shiny::observe({
      if(!identical(model_path_interface_LM(),character(0))){
        if(is.null(interface_architecture()[[1]]) &
           is.null(interface_architecture()[[2]])){
          shinyWidgets::show_alert(
            title = "Error",
            text = "There is no model to load in the directory.",
            type = "error")
        }
      }
    })

    output$lm_interface_setting<-shiny::renderUI({
      if(length(interface_architecture()[[2]])>0){
        ui<-shinydashboard::box(title = "Interface Setting",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                shiny::fluidRow(
                  shiny::column(width = 6,
                         shiny::textInput(inputId = "lm_model_name",
                                   label = "Name"),
                         shiny::textInput(inputId = "lm_model_label",
                                   label = "Label"),
                         shiny::textInput(inputId = "lm_model_version",
                                   label = "Version"),
                         shiny::textInput(inputId = "lm_model_language",
                                   label = "Language")
                  ),
                  shiny::column(width = 6,
                         shiny::sliderInput(inputId = "lm_chunks",
                                     label="N Chunks",
                                     value=1,
                                     min = 1,
                                     max= 50,
                                     step = 1),
                         shiny::sliderInput(inputId = "lm_max_length",
                                     label=paste("Maximal Sequence Length","(Max:",interface_architecture()[2],")"),
                                     value=interface_architecture()[[2]],
                                     min = 20,
                                     max= interface_architecture()[[2]],
                                     step = 1),
                         shiny::sliderInput(inputId = "lm_overlap",
                                     label=paste("N Token Overlap","(Max:",interface_architecture()[2],")"),
                                     value=0,
                                     min = 0,
                                     max= interface_architecture()[[2]],
                                     step = 1),
                         shiny::selectInput(inputId = "lm_aggregation",
                                     label = "Aggregation Hidden States",
                                     choices = c("last",
                                                 "second_to_last",
                                                 "fourth_to_last",
                                                 "all",
                                                 "last_four"))
                  )
                )
        )
        ui_creation<-shinydashboard::box(title = "Creation",
                         width = 12,
                         solidHeader = TRUE,
                         status = "success",
                         shiny::tags$p("Please select a directory where to save the interface."),
                         shinyFiles::shinyDirButton(id="lm_db_select_interface_destination",
                                        label = "Choose a Directory",
                                        title = "Please choose a directory",
                                        icon=shiny::icon("folder-open")),
                         shiny::textInput(inputId =  "lm_db_select_interface_destination_path",
                                   label = shiny::tags$p(shiny::icon("folder"),"Directory Path")),
                         shiny::tags$p("A folder is created within that directory. Please
                                provide a name for the folder."),
                         shiny::textInput(inputId =  "lm_db_select_interface_destination_dir_name",
                                   label = shiny::tags$p(shiny::icon("folder"),"Folder Name:")),
                         shiny::actionButton(inputId = "lm_save_interface",
                                      label = "Save Interface",
                                      icon = shiny::icon("floppy-disk"))
        )
        return(list(ui,ui_creation))
      } else {
        return(NULL)
      }

    })

    shinyFiles::shinyDirChoose(input=input,
                   id="lm_db_select_interface_destination",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = TRUE)
    shiny::observeEvent(input$lm_db_select_interface_destination,{
      shiny::updateTextInput(inputId = "lm_db_select_interface_destination_path",
                      value = shinyFiles::parseDirPath(volumes,input$lm_db_select_interface_destination))
    })

    #Create the interface
    shiny::observeEvent(input$lm_save_interface,{
      model_architecture=interface_architecture()[1]
      if(model_architecture=="BertModel"){
        method="bert"
      } else if(model_architecture=="FunnelModel"){
        method="funnel"
      } else if(model_architecture=="LongformerModel"){
        method="longformer"
      } else if(model_architecture=="RobertaModel"){
        method="roberta"
      } else if(model_architecture=="DebertaV2Model"){
        method="deberta_v2"
      }

      #Check for errors
      error_list=NULL
      if(!dir.exists(input$lm_db_select_interface_destination_path)){
        error_list[length(error_list)+1]=list(shiny::tags$p("The destination directory does not
                                                   exist. Please check your directory path
                                                   and/or create that directory."))
      }

      if(length(error_list)==0){
        shinyWidgets::show_alert(title="Working",
                   text = "Please wait",
                   type="info")

        new_interface=TextEmbeddingModel$new(
          model_name = input$lm_model_name,
          model_label = input$lm_model_label,
          model_language = input$lm_model_language,
          model_version = input$lm_model_version,
          max_length = input$lm_max_length,
          overlap = input$lm_overlap,
          chunks = input$lm_chunks,
          aggregation = input$lm_aggregation,
          ml_framework = input$config_ml_framework,
          model_dir = model_path_interface_LM(),
          method = method)

        save_ai_model(model = new_interface,
                      model_dir = input$lm_db_select_interface_destination_path,
                      dir_name=input$lm_db_select_interface_destination_dir_name)
        rm(new_interface)
        gc()
        shinyWidgets::closeSweetAlert()
      } else {
        shinyWidgets::show_alert(title = "Error",
                   text=shiny::tagList(error_list),
                   type = "error")
        #error_modal<-shiny::modalDialog(
        #  title = "Error",
        #  size = "l",
        #  easyClose = TRUE,
        # footer = shiny::modalButton("Close"),
        # shiny::tagList(error_list)
        #)
        #shiny::showModal(error_modal)
      }

    })

    #Use Language Model Page----------------------------------------------------

    #Choose Model
    shinyFiles::shinyDirChoose(input=input,
                   id="lm_db_select_model_for_use",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = FALSE)
    LanguageModel_for_Use<-shiny::eventReactive(input$lm_db_select_model_for_use,{
      model_path=shinyFiles::parseDirPath(volumes,input$lm_db_select_model_for_use)
      if(length(model_path)>0){
        shinyWidgets::show_alert(title="Loading",
                   text = "Please wait",
                   type="info")
        model=try(load_ai_model(model_dir = model_path,
                                ml_framework=input$config_ml_framework),
                  silent = TRUE)
        if(methods::is(model,class2 = "try-error")==FALSE){
          if("TextEmbeddingModel"%in%class(model)){
            if(utils::compareVersion(as.character(model$get_package_versions()$aifeducation),"0.3.1")>=0){
              shinyWidgets::closeSweetAlert()
              return(model)
            } else {
              shinyWidgets::show_alert(title = "Error",
                         text = paste("The model was created with aifeducation version",
                                      as.character(model$get_package_versions()$aifeducation,"."),
                                      "Uster interface supports only models created with aifeducation version 0.3.1 or later."),
                         type = "error")
              return(NULL)
            }
          } else {
            shinyWidgets::show_alert(title = "Error",
                       text = "The file does not contain an object of class TextEmbeddingModel.",
                       type = "error")
            return(NULL)
          }
        } else {
          shinyWidgets::show_alert(title = "Error",
                     text = model,
                     type = "error")
          return(NULL)
        }
      }
    })

    #Header
    output$lm_use_selected_model_label<-shiny::renderText({
      if(is.null(LanguageModel_for_Use())){
        return(NULL)
      }else{
        return(LanguageModel_for_Use()$get_model_info()$model_label)
      }
    })

    #Description
    output$lm_desc_abstract_and_desc<-shiny::renderUI({
      model=LanguageModel_for_Use()
      if(!is.null(model)){
        language_eng=input$lm_desc_language_select
        if(language_eng==TRUE){
          ui<-list(
            shiny::tags$h3("Abstract"),
            if(!is.null(model$get_model_description()$abstract_eng)){
              shiny::tags$p(shiny::includeMarkdown(model$get_model_description()$abstract_eng))
            },
            shiny::tags$h3("Description"),
            if(!is.null(model$get_model_description()$eng)){
              shiny::tags$p(shiny::includeMarkdown(model$get_model_description()$eng))
            }
          )
        } else {
          ui<-list(
            shiny::tags$h3("Abstract"),
            if(!is.null(model$get_model_description()$abstract_native)){
              shiny::tags$p(shiny::includeMarkdown(model$get_model_description()$abstract_native))
            },
            shiny::tags$h3("Description"),
            if(!is.null(model$get_model_description()$native)){
              shiny::tags$p(shiny::includeMarkdown(model$get_model_description()$native))
            }
          )
        }
        return(ui)
      } else {
        return(NULL)
      }
    })

    #Routines for Embeddings
    shinyFiles::shinyFileChoose(input=input,
                    id="lm_choose_file_raw_texts_for_embed",
                    roots = volumes,
                    filetypes="rda")
    shiny::observeEvent(input$lm_choose_file_raw_texts_for_embed,{
      tmp_file_path=shinyFiles::parseFilePaths(volumes,input$lm_choose_file_raw_texts_for_embed)
      if(nrow(tmp_file_path)>0){
        shiny::updateTextInput(inputId="lm_choose_file_raw_texts_for_embed_path",
                        value = tmp_file_path[[1,"datapath"]])
      }
    })

    shinyFiles::shinyDirChoose(input = input,
                   id="lm_choose_file_path_for_embeddings",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = TRUE)
    shiny::observeEvent(input$lm_choose_file_path_for_embeddings,{
      shiny::updateTextInput(inputId = "lm_choose_file_path_for_embeddings_path",
                      value = shinyFiles::parseDirPath(volumes,input$lm_choose_file_path_for_embeddings))
    })

    shiny::observeEvent(input$lm_embedd_start,{
      #Check input
      error_list=NULL
      file_path_raw_texts=input$lm_choose_file_raw_texts_for_embed_path
      if(!file.exists(file_path_raw_texts)){
        error_list[length(error_list)+1]=shiny::tags$p("File for raw texts does not exist. Please check your path.")
      }

      directory_path_embeddings=input$lm_choose_file_path_for_embeddings_path
      file_path_embeddings=paste0(directory_path_embeddings,"/",
                                  input$lm_choose_file_path_for_embeddings_file_name,
                                  ".rda")
      if(!file.exists(directory_path_embeddings)){
        error_list[length(error_list)+1]=shiny::tags$p("Directory for saving the embeddings does not exist.
      Please check your path or create the directory.")
      }

      tmp_text_data=load(file_path_raw_texts)
      if(!("id"%in%colnames(tmp_text_data))){
        error_list[length(error_list)+1]=shiny::tags$p("The file for raw texts does not contain a
      column 'id'.")
      }
      if(!("text"%in%colnames(tmp_text_data))){
        error_list[length(error_list)+1]=shiny::tags$p("The file for raw texts does not contain a
      column 'text'.")
      }

      if(length(error_list)==0){
        model=LanguageModel_for_Use()
        embeddings=model$embed(
          raw_text=tmp_text_data$text,
          doc_id=tmp_text_data$id,
          batch_size=input$lm_embed_batch_size,
          trace=FALSE)

        save(embeddings,
             file = file_path_embeddings)
        rm(embeddings)
        gc()
      } else {
        error_modal<-shiny::modalDialog(
          title = "Error",
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton("Close"),
          shiny::tagList(error_list)
        )
        shiny::showModal(error_modal)
      }
    })

    #Routine for decode, encode, tokenize
    lm_encodings<-shiny::eventReactive(input$lm_encode_start,{
      model=LanguageModel_for_Use()

      integer_sequence=model$encode(
        raw_text = input$lm_text_for_encode,
        token_encodings_only = TRUE,
        to_int=TRUE,
        trace=FALSE)[[1]]

      integer_output=NULL
      for(i in 1:length(integer_sequence)){
        tmp_sequence=paste(integer_sequence[[i]],collapse = " ")
        integer_output[length(integer_output)+1]=list(shiny::tags$p(shiny::tags$b(paste("Chunk",i))))
        integer_output[length(integer_output)+1]=list(shiny::tags$p(tmp_sequence))
      }

      token_sequence=model$encode(
        raw_text = input$lm_text_for_encode,
        token_encodings_only = TRUE,
        to_int=FALSE,
        trace=FALSE)[[1]]

      token_output=NULL
      for(i in 1:length(token_sequence)){
        tmp_sequence=paste(token_sequence[[i]],collapse = " ")
        token_output[length(token_output)+1]=list(shiny::tags$p(shiny::tags$b(paste("Chunk",i))))
        token_output[length(token_output)+1]=list(shiny::tags$p(tmp_sequence))
      }

      return(list(integer_encodings=integer_output,
                  token_encodings=token_output))
    })

    output$lm_txt_to_int=shiny::renderUI(lm_encodings()$integer_encodings)
    output$lm_txt_to_tokens=shiny::renderUI(lm_encodings()$token_encodings)

    shiny::observeEvent(input$lm_encode_clear,{
      shiny::updateTextAreaInput(inputId = "lm_text_for_encode",value = "")
      output$lm_txt_to_int=shiny::renderUI(NULL)
      output$lm_txt_to_tokens=shiny::renderUI(NULL)
    })

    lm_decodings<-shiny::eventReactive(input$lm_decode_start,{
      model=LanguageModel_for_Use()

      int_sequence=stringr::str_extract_all(input$lm_ids_for_decode, "\\d+")

      output_list_text=model$decode(int_sequence,
                                    to_token=FALSE)
      output_list_token=model$decode(int_sequence,
                                     to_token=TRUE)
      text_list=NULL
      token_list=NULL

      for(i in 1:length(output_list_text)){
        text_list[length(text_list)+1]=list(shiny::tags$p(paste("Chunk",i)))
        text_list[length(text_list)+1]=list(shiny::tags$p(output_list_text[[i]]))

        token_list[length(token_list)+1]=list(shiny::tags$p(paste("Chunk",i)))
        token_list[length(token_list)+1]=list(shiny::tags$p(output_list_token[[i]]))
      }

      return(list(text_decode=text_list,
                  token_decode=token_list))
    })

    output$lm_ids_to_txt<-shiny::renderUI(lm_decodings()$text_decode)
    output$lm_ids_to_tokens<-shiny::renderUI(lm_decodings()$token_decode)

    shiny::observeEvent(input$lm_decode_clear,{
      shiny::updateTextAreaInput(inputId = "lm_ids_for_decode",value = "")
      output$lm_ids_to_txt=shiny::renderUI(NULL)
      output$lm_ids_to_tokens=shiny::renderUI(NULL)
    })

    #Routine for fill mask
    fill_masked_solutions=shiny::eventReactive(input$lm_fill_mask_start,{
      model=LanguageModel_for_Use()

      solutions=try(
        model$fill_mask(
          text=input$lm_txt_for_fill_mask,
          n_solutions=input$lm_n_fillments_for_fill_mask),
        silent = TRUE)

      if(methods::is(solutions,class2 = "try-error")==FALSE){
        shiny::updateNumericInput(inputId = "lm_select_mask_for_fill_mask",
                           max=length(solutions))

        return(solutions)
      } else {
        shinyWidgets::show_alert(title = "Error",
                   text = "Text does not contain at least one mask token. Please
                 check your input.",
                   type = "error")
        return(NULL)
      }


    })

    output$lm_scores_for_fill_mask<-shiny::renderPlot({
      plot_data=fill_masked_solutions()[[input$lm_select_mask_for_fill_mask]]
      plot_data=plot_data[order(plot_data$score,decreasing=FALSE),]
      plot_data$token_str=factor(plot_data$token_str,levels=(plot_data$token_str))
      plot=ggplot2::ggplot(data = plot_data)+
        ggplot2::geom_col(ggplot2::aes(x=rlang::.data$token_str,y=rlang::.data$score))+
        ggplot2::coord_flip()+
        ggplot2::xlab("tokens")+
        ggplot2::ylab("score")+
        ggplot2::theme_classic()+
        ggplot2::theme(text = ggplot2::element_text(size = input$lm_mask_plot_text_size))
      return(plot)
    },
    res = 2*72)

    #TabPanels
    output$lm_use_tabs<-shiny::renderUI({
      if(!is.null(LanguageModel_for_Use())){
        model=LanguageModel_for_Use()
        ui<-shinydashboard::tabBox(width = 12,
                   #Model Description--------------------------------------------------
                   shiny::tabPanel("Model Description",
                            shiny::tags$h3(model$get_model_info()$model_label),
                            shiny::tags$p("Developers: ",paste(format(x=model$get_publication_info()$developed_by$authors,
                                                               include = c("given","family")),
                                                        collapse = ", ")),
                            shiny::tags$p("Citation: ",model$pub_info$developed_by$citation),
                            if(!is.null(model$pub_info$modifided_by$authors)){
                              shiny::tags$p("Modifiers: ",paste(format(x=model$pub_info$modifided_by$authors,
                                                                include = c("given","family")),
                                                         collapse = ", "))
                            },
                            if(!is.null(model$pub_info$modifided_by$citation)){
                              shiny::tags$p("Citation: ",model$pub_info$modifided_by$citation)
                            },
                            if(!is.null(model$pub_info$modifided_by$citation)){
                              shiny::tags$p("Language: ",model$get_model_info()$model_language)
                            },
                            shinyWidgets::switchInput(
                              inputId = "lm_desc_language_select",
                              label = "Language",
                              onLabel="English",
                              offLabel = "Native",
                              value = TRUE,
                              labelWidth = "80px"
                            ),
                            shiny::fluidRow(
                              shiny::column(width=6,
                                     shiny::uiOutput(outputId = "lm_desc_abstract_and_desc")
                              ),
                              shiny::column(width=6,
                                     shiny::tags$h3("Configuration"),
                                     shiny::tags$p("Method: ",model$get_model_info()$model_method),
                                     shiny::tags$p("Max Tokens per Chunk: ",model$get_model_info()$model_max_size),
                                     shiny::tags$p("Max Chunks: ",model$get_transformer_components()$chunks),
                                     shiny::tags$p("Token Overlap: ",model$get_transformer_components()$overlap),
                                     shiny::tags$p("Max Tokens: ",(model$get_model_info()$model_max_size-model$get_transformer_components()$overlap)
                                            *model$get_transformer_components()$chunks+model$get_model_info()$model_max_size),
                                     shiny::tags$p("Hidden States Aggregation: ",model$get_transformer_components()$aggregation),
                                     shiny::tags$h3("Sustainability"),
                                     if(methods::isClass(Class="data.frame",where = model$get_sustainability_data())){
                                       if(is.na(model$get_sustainability_data()[1,1])==FALSE){
                                         shiny::tags$p("Energy Consumption (kWh): ",sum(model$get_sustainability_data()[,"sustainability_data.total_energy_kwh"]))
                                       } else {
                                         shiny::tags$p("Energy Consumption (kWh): ","not estimated")
                                       }
                                     } else {
                                       shiny::tags$p("Energy Consumption (kWh): ","not estimated")
                                     },
                                     if(methods::isClass(Class="data.frame",where = model$get_sustainability_data())){
                                       if(is.na(model$get_sustainability_data()[1,1])==FALSE){
                                         shiny::tags$p("Carbon Footprint (CO2eq. kg): ",sum(model$get_sustainability_data()[,"sustainability_data.co2eq_kg"]))
                                       } else {
                                         shiny::tags$p("Carbon Footprint (CO2eq. kg): ","not estimated")
                                       }
                                     } else {
                                       shiny::tags$p("Carbon Footprint (CO2eq. kg): ","not estimated")
                                     }
                              )
                            )

                   ),
                   #Create Text Embeddings---------------------------------------------
                   shiny::tabPanel("Create Text Embeddings",
                            shiny::fluidRow(
                              shinydashboard::box(title = "Raw Texts",
                                  solidHeader = TRUE,
                                  status = "primary",
                                  shinyFiles::shinyFilesButton(id="lm_choose_file_raw_texts_for_embed",
                                                   label="Choose File",
                                                   title="Please choose a file",
                                                   icon=shiny::icon("file"),
                                                   multiple=FALSE),
                                  shiny::textInput(inputId = "lm_choose_file_raw_texts_for_embed_path",
                                            label = shiny::tags$p(shiny::icon("file"),"File Path"))
                              ),
                              shinydashboard::box(title = "Text Embeddings Destination",
                                  solidHeader = TRUE,
                                  status = "primary",
                                  shinyFiles::shinyDirButton(id="lm_choose_file_path_for_embeddings",
                                                 title="Choose a folder for storing the embeddings",
                                                 label = "Choose Folder",
                                                 icon=shiny::icon("folder-open")),
                                  shiny::textInput(inputId = "lm_choose_file_path_for_embeddings_path",
                                            label = shiny::tags$p(shiny::icon("folder"),"Path to Folder")),
                                  shiny::textInput(inputId = "lm_choose_file_path_for_embeddings_file_name",
                                            label = shiny::tags$p(shiny::icon("file"),"File Name")),
                                  shiny::numericInput(inputId = "lm_embed_batch_size",
                                               label = "Batch Size",
                                               min = 1,
                                               max = 512,
                                               value = 8),
                                  shiny::actionButton(inputId = "lm_embedd_start",
                                               label = "Start Embedding Texts",
                                               icon = shiny::icon("paper-plane"))
                              )
                            )
                   ),
                   #Encode/Decode/Tokenize---------------------------------------------
                   shiny::tabPanel("Encode/Decode/Tokenize",
                            shiny::fluidRow(
                              shinydashboard::box(
                                title = "Encode",
                                status = "primary",
                                solidHeader = TRUE,
                                width = 12,
                                shinydashboard::box(width = 4,
                                    title = "Raw Text",
                                    solidHeader=TRUE,
                                    shiny::textAreaInput(inputId = "lm_text_for_encode",
                                                  label = NULL,
                                                  rows=5),
                                    shiny::actionButton(inputId = "lm_encode_start",
                                                 label = "Encode",
                                                 width = "100%",
                                                 icon = shiny::icon("paper-plane")),
                                    shiny::actionButton(inputId = "lm_encode_clear",
                                                 label = "Clear",
                                                 width = "100%",
                                                 icon = shiny::icon("trash"))
                                ),
                                shinydashboard::box(width = 4,
                                    title = "Token Sequence",
                                    solidHeader=TRUE,
                                    shiny::uiOutput(outputId="lm_txt_to_tokens")
                                ),
                                shinydashboard::box(width = 4,
                                    title = "ID Sequence",
                                    solidHeader=TRUE,
                                    shiny::uiOutput(outputId="lm_txt_to_int")
                                )
                              )
                            ),
                            shiny::fluidRow(
                              shinydashboard::box(
                                title = "Decode",
                                status = "primary",
                                solidHeader = TRUE,
                                width = 12,
                                shinydashboard::box(width = 4,
                                    title = "ID Sequence",
                                    solidHeader = TRUE,
                                    shiny::textAreaInput(inputId = "lm_ids_for_decode",
                                                  label = NULL,
                                                  rows=5),
                                    shiny::actionButton(inputId = "lm_decode_start",
                                                 label = "Decode",
                                                 width = "100%",
                                                 icon = shiny::icon("paper-plane")),
                                    shiny::actionButton(inputId = "lm_decode_clear",
                                                 label = "Clear",
                                                 width = "100%",
                                                 icon = shiny::icon("trash"))
                                ),
                                shinydashboard::box(width = 4,
                                    title = "Token Sequence",
                                    solidHeader=TRUE,
                                    shiny::uiOutput(outputId="lm_ids_to_tokens")
                                ),
                                shinydashboard::box(width = 4,
                                    title = "Raw Text",
                                    solidHeader=TRUE,
                                    shiny::uiOutput(outputId="lm_ids_to_txt")
                                )
                              )
                            )

                   ),
                   #Fill Mask---------------------------------------------------------
                   shiny::tabPanel("Fill Mask",
                            shiny::fluidRow(
                              shinydashboard::box(title = "Text Sequence",
                                  solidHeader = TRUE,
                                  status = "primary",
                                  width = 6,
                                  shiny::renderTable(model$get_special_tokens()),
                                  shiny::textAreaInput(inputId = "lm_txt_for_fill_mask",
                                                rows = 5,
                                                label="Text"),
                                  shiny::numericInput(inputId = "lm_n_fillments_for_fill_mask",
                                               label = "N Solutions per mask",
                                               value = 5,
                                               min = 1,
                                               max = 50),
                                  shiny::actionButton(inputId="lm_fill_mask_start",
                                               label =  "Calculate Tokens",
                                               width = "100%",
                                               icon = shiny::icon("paper-plane"))
                              ),
                              shinydashboard::box(title = "Estimated Tokens",
                                  solidHeader = TRUE,
                                  status = "primary",
                                  width = 6,
                                  shiny::sliderInput(inputId = "lm_mask_plot_text_size",
                                              min = 1,
                                              max = 20,
                                              value = 10,
                                              step = 0.5,
                                              label = "Text Size"),
                                  shiny::numericInput(inputId = "lm_select_mask_for_fill_mask",
                                               value = 1,
                                               min = 1,
                                               max = 1,
                                               label = "Select Mask Token"),
                                  shiny::plotOutput(outputId = "lm_scores_for_fill_mask"))
                            )
                   )
        )
        return(ui)
      } else {
        return(NULL)
      }
    })

    #Document Page--------------------------------------------------------------
    shinyFiles::shinyDirChoose(input=input,
                   id="lm_db_select_model_for_documentation",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = FALSE)
    lm_interface_for_documentation_path=shiny::eventReactive(input$lm_db_select_model_for_documentation,{
      path=shinyFiles::parseDirPath(volumes,input$lm_db_select_model_for_documentation)
      return(path)
    })

    LanguageModel_for_Documentation<-shiny::eventReactive(lm_interface_for_documentation_path(),{
      if(length(lm_interface_for_documentation_path())>0){
        shinyWidgets::show_alert(title="Working",
                   text = "Please wait",
                   type="info")
        model=try(load_ai_model(model_dir = lm_interface_for_documentation_path(),
                                ml_framework=input$config_ml_framework),silent = TRUE)
        if(methods::is(model,class2 = "try-error")==FALSE){
          if("TextEmbeddingModel"%in%class(model)){
            if(utils::compareVersion(as.character(model$get_package_versions()$aifeducation),"0.3.1")>=0){
              shinyWidgets::closeSweetAlert()
              return(model)
            } else {
              shinyWidgets::show_alert(title = "Error",
                         text = paste("The model was created with aifeducation version",
                                      as.character(model$get_package_versions()$aifeducation,"."),
                                      "Uster interface supports only models created with aifeducation version 0.3.1 or later."),
                         type = "error")
              return(NULL)
            }
          } else {
            shinyWidgets::show_alert(title = "Error",
                       text = "The directory does not contain an object of class TextEmbeddingModel.
                   Please check your directory.",
                       type = "error")
            return(NULL)
          }
        } else {
          shinyWidgets::show_alert(title = "Error",
                     text = model,
                     type = "error")
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })

    output$lm_document_selected_model_label<-shiny::renderText({
      model<-LanguageModel_for_Documentation()
      if(is.null(model)){
        return(NULL)
      } else {
        return(model$get_model_info()$model_label)
      }
    })

    output$lm_document_tabs<-shiny::renderUI({
      model<-LanguageModel_for_Documentation()
      if(is.null(model)){
        return(NULL)
      } else {

        ui_parts=shiny::tagList()
        pup_info_for=c("developed_by","modified_by")
        pup_info_titles=c("Developers","Modifiers")
        for(i in 1:length(pup_info_for)){
          widgets=NULL
          for(j in 1:10){
            pup_info=model$get_publication_info()[[pup_info_for[i]]]$authors
            widgets[[j]]=list(
              shiny::fluidRow(
                shiny::column(width = 4,
                       shiny::textInput(inputId = paste0("lm_doc_",pup_info_titles[i],"_fist_name_",j),
                                 label = paste("Given Name",j),
                                 value = pup_info[[j]]$given,
                                 width = "100%")
                ),
                shiny::column(width = 4,
                       shiny::textInput(inputId = paste0("lm_doc_",pup_info_titles[i],"_last_name_",j),
                                 label = paste("Family Name",j),
                                 value = pup_info[[j]]$family,
                                 width = "100%")
                ),
                shiny::column(width = 4,
                       shiny::textInput(inputId = paste0("lm_doc_",pup_info_titles[i],"_mail_",j),
                                 label = paste("Mail",j),
                                 value = pup_info[[j]]$email,
                                 width = "100%")
                )
              )
            )
          }
          ui_parts[length(ui_parts)+1]=list(
            shiny::tabPanel(title = pup_info_titles[i],
                     widgets,
                     shiny::textInput(inputId = paste0("lm_doc_",pup_info_for[i],"_citation"),
                               label = "Citation",
                               value = model$get_publication_info()[[pup_info_for[i]]]$citation),
                     shiny::textInput(inputId = paste0("lm_doc_",pup_info_for[i],"_url"),
                               label = "URL",
                               value = model$get_publication_info()[[pup_info_for[i]]]$url),
                     shiny::actionButton(inputId = paste0("lm_doc_",pup_info_for[i],"_save"),
                                  label = "Save",
                                  icon = shiny::icon("floppy-disk"))
            )
          )
        }

        documention_part=c("abstract_eng","abstract_native","description_eng","description_native")
        documention_titles=c("Abstract English","Abstract Native","Description English","Description Native")
        documentation_field=c("abstract_eng","abstract_native","eng","native")
        documentation_keywords=c("keywords_eng","keywords_native")
        for(i in 1:length(documention_part)){

          tmp_tabPanel<-shiny::tabPanel(
            title = documention_titles[i],
            shiny::fluidRow(
              shiny::column(width = 6,
                     shiny::textAreaInput(inputId = paste0("lm_doc_editor_",documention_part[i]),
                                   label = "Editor",
                                   rows = 6,
                                   value = model$get_model_description()[[documentation_field[i]]]
                     ),
                     if(i<=2){
                       shiny::textInput(inputId = paste0("lm_doc_editor_",documention_part[i],"_keywords"),
                                 value = model$get_model_description()[[documentation_keywords[i]]],
                                 label = "Keywords")
                     },
                     shiny::actionButton(inputId = paste0("lm_doc_editor_",documention_part[i],"_preview_button"),
                                  label = "Preview",
                                  icon = shiny::icon("eye")),
                     shiny::actionButton(inputId = paste0("lm_doc_editor_",documention_part[i],"_save_button"),
                                  label = "Save",
                                  icon = shiny::icon("floppy-disk"))),
              shiny::column(width = 6,
                     shiny::tags$p(shiny::tags$b("Preview")),
                     shiny::uiOutput(outputId = paste0("lm_doc_editor_",documention_part[i],"_preview")))
            )
          )
          ui_parts[length(ui_parts)+1]=list(tmp_tabPanel)
        }

        ui<-shinydashboard::tabBox(width = 12,
                   ui_parts[[1]],
                   ui_parts[[2]],
                   ui_parts[[3]],
                   ui_parts[[4]],
                   ui_parts[[5]],
                   ui_parts[[6]])
        return(ui)
      }

    })

    #Preview Events
    shiny::observeEvent(input$lm_doc_editor_abstract_eng_preview_button,{
      output$lm_doc_editor_abstract_eng_preview<-shiny::renderUI({
        return(shiny::includeMarkdown(input$lm_doc_editor_abstract_eng))
      })
    })
    shiny::observeEvent(input$lm_doc_editor_abstract_native_preview_button,{
      output$lm_doc_editor_abstract_native_preview<-shiny::renderUI({
        return(shiny::includeMarkdown(input$lm_doc_editor_abstract_native))
      })
    })
    shiny::observeEvent(input$lm_doc_editor_description_eng_preview_button,{
      output$lm_doc_editor_description_eng_preview<-shiny::renderUI({
        return(shiny::includeMarkdown(input$lm_doc_editor_description_eng))
      })
    })
    shiny::observeEvent(input$lm_doc_editor_description_native_preview_button,{
      output$lm_doc_editor_description_native_preview<-shiny::renderUI({
        return(shiny::includeMarkdown(input$lm_doc_editor_description_native))
      })
    })


    #Save Events
    #Developers
    shiny::observeEvent(input$lm_doc_developed_by_save,{
      model<-LanguageModel_for_Documentation()

      tmp_person_list=NULL
      for(i in 1:10){
        given=input[[paste0("lm_doc_","Developers","_fist_name_",i)]]
        family=input[[paste0("lm_doc_","Developers","_last_name_",i)]]
        mail=input[[paste0("lm_doc_","Developers","_mail_",i)]]
        if((!is.null(given) & !(given="")) &
           (!is.null(family) & !(family=""))){
          person=person(given=given,family=family,email=mail)
          tmp_person_list=append(x=tmp_person_list,
                                 values = person)
        }
      }

      model$set_publication_info(type="developer",
                                 authors = tmp_person_list,
                                 citation = input[[paste0("lm_doc_","developed_by","_citation")]],
                                 url = input[[paste0("lm_doc_","developed_by","_url")]])
      r_interface_path=paste0(lm_interface_for_documentation_path(),"/r_interface.rda")
      save(model,file = r_interface_path)
      LanguageModel_for_Documentation<-shiny::reactive({model})

    })

    #Modifiers
    shiny::observeEvent(input$lm_doc_modified_by_save,{
      model<-LanguageModel_for_Documentation()

      tmp_person_list=NULL
      for(i in 1:10){
        given=input[[paste0("lm_doc_","Modifiers","_fist_name_",i)]]
        family=input[[paste0("lm_doc_","Modifiers","_last_name_",i)]]
        mail=input[[paste0("lm_doc_","Modifiers","_mail_",i)]]
        if((!is.null(given) & !(given="")) &
           (!is.null(family) & !(family=""))){
          person=person(given=given,family=family,email=mail)
          tmp_person_list=append(x=tmp_person_list,
                                 values = person)
        }
      }

      model$set_publication_info(type="modifier",
                                 authors = tmp_person_list,
                                 citation = input[[paste0("lm_doc_","modified_by","_citation")]],
                                 url = input[[paste0("lm_doc_","modified_by","_url")]])
      r_interface_path=paste0(lm_interface_for_documentation_path(),"/r_interface.rda")
      save(model,file = r_interface_path)
      LanguageModel_for_Documentation<-shiny::reactive({model})
    })

    shiny::observeEvent(input$lm_doc_editor_abstract_eng_save_button,{
      model<-LanguageModel_for_Documentation()
      model$set_model_description(
        abstract_eng=input$lm_doc_editor_abstract_eng,
        keywords_eng=input$lm_doc_editor_abstract_eng_keywords)
      r_interface_path=paste0(lm_interface_for_documentation_path(),"/r_interface.rda")
      save(model,file = r_interface_path)
      LanguageModel_for_Documentation<-shiny::reactive({model})
    })

    shiny::observeEvent(input$lm_doc_editor_abstract_native_save_button,{
      model<-LanguageModel_for_Documentation()
      model$set_model_description(
        abstract_native=input$lm_doc_editor_abstract_native,
        keywords_native=input$lm_doc_editor_abstract_native_keywords)
      r_interface_path=paste0(lm_interface_for_documentation_path(),"/r_interface.rda")
      save(model,file = r_interface_path)
      LanguageModel_for_Documentation<-shiny::reactive({model})

    })
    shiny::observeEvent(input$lm_doc_editor_description_eng_save_button,{
      model<-LanguageModel_for_Documentation()
      model$set_model_description(
        eng =input$lm_doc_editor_description_eng)
      r_interface_path=paste0(lm_interface_for_documentation_path(),"/r_interface.rda")
      save(model,file = r_interface_path)
      LanguageModel_for_Documentation<-shiny::reactive({model})
    })
    shiny::observeEvent(input$lm_doc_editor_description_native_save_button,{
      model<-LanguageModel_for_Documentation()
      model$set_model_description(
        native =input$lm_doc_editor_description_native)
      r_interface_path=paste0(lm_interface_for_documentation_path(),"/r_interface.rda")
      save(model,file = r_interface_path)
      LanguageModel_for_Documentation<-shiny::reactive({model})
    })



    #TextEmbeddingClassifier Rounties--------------------------------------------
    #Create and Train Page-------------------------------------------------------
    #Select and load embedding
    shinyFiles::shinyFileChoose(input=input,
                    id="tec_select_embeddings_for_training",
                    roots = volumes,
                    filetype=c("rda","rdata"))

    tec_embeddings_for_train_path=shiny::eventReactive(input$tec_select_embeddings_for_training,{
      tmp_file_path=shinyFiles::parseFilePaths(volumes,input$tec_select_embeddings_for_training)
      if(nrow(tmp_file_path)>0){
        return(tmp_file_path[[1,"datapath"]])
      } else {
        return(NULL)
      }
    },ignoreNULL = FALSE)


    tec_embeddings_for_training=shiny::reactive({
      file_path=tec_embeddings_for_train_path()
      if(!is.null(file_path)){
        if(file.exists(file_path)==TRUE){
          shinyWidgets::show_alert(title="Loading",
                     text = "Please wait",
                     type="info")
          file=load(file_path)
          embeddings=get(x=file)
          if(("EmbeddedText" %in% class(embeddings))==TRUE){
            shinyWidgets::closeSweetAlert()
            return(embeddings)
          } else {
            shinyWidgets::closeSweetAlert()
            shinyWidgets::show_alert(title="Error",
                       text = "The file contains data in an unsupported format.
                     Text embeddings must be of class 'EmbeddedText'. Please
                     check data. Data embeddings should always be created via data
                     preparation of this user interfache or with the corresponding
                     method of the TextEmbeddingModel.",
                       type="error")
            rm(embeddings)
            gc()
            return(NULL)
          }
        } else {
          shinyWidgets::closeSweetAlert()
          shinyWidgets::show_alert(title="Error",
                     text = "The file does not exist on the path.",
                     type="error")
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })

    output$tec_embeddings_for_training_overview<-shiny::renderUI({
      embeddings=tec_embeddings_for_training()
      if(!is.null(embeddings)){
        model_info=embeddings$get_model_info()
        info_table=matrix(nrow = 3,
                          ncol = 4,
                          data = "")
        info_table[1,1]="Model Method:"
        info_table[2,1]="Hidden State Aggregation:"
        info_table[3,1]="Model Language:"

        info_table[1,2]=model_info$model_method
        info_table[2,2]=model_info$param_aggregation
        info_table[3,2]=model_info$model_language

        info_table[1,3]="Tokens per Chunk:"
        info_table[2,3]="Max Chunks:"
        info_table[3,3]="Token Overlap:"

        info_table[1,4]=model_info$param_seq_length
        info_table[2,4]=model_info$param_chunks
        info_table[3,4]=model_info$param_overlap

        ui<-list(
          shinydashboard::valueBox(value=nrow(embeddings$embeddings),
                   subtitle="Number of Cases",
                   icon = shiny::icon("list"),
                   width=12),
          shiny::tags$h3("Model:",  model_info$model_label),
          shiny::tags$p("Name:", model_info$model_name),
          shiny::tags$p("Created", model_info$model_date),
          shiny::renderTable(expr=info_table,
                      colnames=FALSE)
        )
        return(ui)
      } else {
        return(NULL)
      }
    })

    #Select and load Target Data
    shinyFiles::shinyFileChoose(input=input,
                    id="tec_select_target_data_for_training",
                    roots = volumes,
                    filetypes=c("csv","rda","rdata","xlsx"))

    tec_target_data_for_train_path=shiny::eventReactive(input$tec_select_target_data_for_training,{
      tmp_file_path=shinyFiles::parseFilePaths(volumes,input$tec_select_target_data_for_training)
      if(nrow(tmp_file_path)>0){
        return(tmp_file_path[[1,"datapath"]])
      } else {
        return(NULL)
      }
    },ignoreNULL = FALSE)

    tec_target_data_for_training<-shiny::reactive({
      file_path=tec_target_data_for_train_path()
      if(!is.null(file_path)){
        if(file.exists(file_path)==TRUE){
          extension=stringr::str_split_fixed(file_path,pattern="\\.")[1,-1]
          extension=stringr::str_to_lower(extension)
          #extension=stringi::stri_split_fixed(file_path,pattern=".")[[1]]
          #extension=stringi::stri_trans_tolower(extension[[length(extension)]])
          shinyWidgets::show_alert(title="Loading",
                     text = "Please wait",
                     type="info")
          if(extension=="csv"|extension=="txt"){
            target_data=try(as.data.frame(
              utils::read.csv2(file = file_path,
                        header = TRUE)),silent = TRUE)
          } else if(extension=="xlsx"){
            target_data=try(
              as.data.frame(
                readxl::read_xlsx(
                  path=file_path,
                  sheet = 1,
                  col_names = TRUE)),
              silent=TRUE)
          } else if (extension%in%c("rda","rdata")){
            object_name=load(file = file_path)
            target_data=get(x=object_name)
            target_data=try(
              as.data.frame(target_data),
              silent = TRUE)
          } else {
            target_data=NA
          }

          #Final Check
          if(is.character(target_data)){
            shinyWidgets::closeSweetAlert()
            shinyWidgets::show_alert(title="Error",
                       text = "Data can not be loaded as data frame. Please
                     check your data.",
                       type="error")
            return(NULL)
          } else {
            if("id"%in%colnames(target_data)){
              shinyWidgets::closeSweetAlert()
              return(target_data)
            } else {
              shinyWidgets::closeSweetAlert()
              shinyWidgets::show_alert(title="Error",
                         text = "Data does not contain a column named 'id'. This
                       column is necessary to match the text embeddings to their
                       corresponding targets. Please check your data.",
                         type="error")
              return(NULL)
            }
          }
        } else {
          shinyWidgets::closeSweetAlert()
          shinyWidgets::show_alert(title="Error",
                     text = "The file does not exist on the path.",
                     type="error")
          return(NULL)
        }

      } else {
        return(NULL)
      }
    })

    output$tec_target_data_for_training_overview<-shiny::renderUI({
      target_data=tec_target_data_for_training()
      if(!is.null(target_data)){
        column_names=colnames(target_data)
        column_names=setdiff(x=column_names,y=c("id","text"))
        ui<-list(
          shinydashboard::valueBox(value = nrow(target_data),
                   subtitle="Number of Cases",
                   icon = shiny::icon("list"),
                   width=12),
          shiny::selectInput(inputId = "tec_target_data_column",
                      label="Select a Column",
                      choices = column_names),
          shiny::tableOutput(outputId = "tec_target_data_abs_freq")
        )
      } else {
        return(NULL)
      }
    })

    output$tec_target_data_abs_freq<-shiny::renderTable({
      relevant_data=tec_target_data_for_training()
      relevant_data=relevant_data[input$tec_target_data_column]
      return(table(relevant_data,useNA = "always"))
    })

    #Architecture
    output$tec_attention_layers_for_training<-shiny::renderUI({
      if(input$tec_attention_type=="multihead"){
        ui<-list(
          shiny::sliderInput(inputId = "tec_self_attention_heads",
                      label = "Number of Self Attention Heads",
                      min = 1,
                      value = 4,
                      max=48,
                      step = 1,
                      round = TRUE)
        )
        return(ui)
      } else {
        return(NULL)
      }
    })

    output$tec_dense_layer_check<-shiny::renderText({
      #as.numeric(stringi::stri_split_regex(input$tec_hidden,pattern=",|[:blank:]")[[1]])
      as.numeric(stringr::str_extract_all(input$tec_hidden,pattern = "\\d+")[[1]])
    })
    output$tec_rec_layer_check<-shiny::renderText({
      #as.numeric(stringi::stri_split_regex(input$tec_rec,pattern=",|[:blank:]")[[1]])
      as.numeric(stringr::str_extract_all(input$tec_rec,pattern = "\\d+")[[1]])
    })

    #Training settings
    output$tec_dynamic_sample_weights<-shiny::renderUI({
      ui<-list(
        shiny::sliderInput(inputId = "tec_bpl_max",
                    label = "Max Certainty Value",
                    value = 1,
                    max = 1,
                    min = input$tec_bpl_anchor,
                    step = 0.01),
        shiny::sliderInput(inputId = "bpl_min",
                    label = "Min Certainty Value",
                    value = 0,
                    max = input$tec_bpl_anchor,
                    min = 0,
                    step = 0.01)
      )
      return(ui)
    })


    #Model Destination
    shinyFiles::shinyDirChoose(input=input,
                   id="tec_create_select_destination_folder",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = FALSE)
    shiny::observeEvent(input$tec_create_select_destination_folder,{
      shiny::updateTextInput(inputId = "tec_create_select_destination_folder_path",
                      value = shinyFiles::parseDirPath(volumes,input$tec_create_select_destination_folder))
    })

    #Test Data matching
    shiny::observeEvent(input$tec_create_test_data_matching,{

      cond_1=(!is.null(tec_embeddings_for_training()))
      cond_2=(!is.null(tec_target_data_for_training()))

      if(cond_1 & cond_2){
        embeddings=tec_embeddings_for_training()
        targets=tec_target_data_for_training()[input$tec_target_data_column]
        matched_cases=intersect(x=rownames(embeddings$embeddings),
                                y=rownames(targets))
        n_matched_cases=length(matched_cases)
        shinyWidgets::show_alert(title = "Matching Results",
                   text = paste(n_matched_cases,
                                "out of",
                                nrow(embeddings$embeddings),
                                "could be matched"),
                   type ="info")
      } else {
        shinyWidgets::show_alert(title = "Error",
                   text = "Embeddings and target data must be selected before matching is possible.",
                   type = "error")
        #error_modal<-shiny::modalDialog(
        #  title = "Error",
        #  size = "l",
        #  easyClose = TRUE,
        #  footer = shiny::modalButton("Close"),
        # shiny::tags$p("Embeddings and target data must be selected before matching is possible."))
        #shiny::showModal(error_modal)
      }
    },ignoreInit = TRUE)

    #Start Creation
    shiny::observeEvent(input$tec_create_start,{
      #Check requirements
      error_list=NULL
      if(is.null(tec_target_data_for_training())==TRUE){
        error_list[length(error_list)+1]=list(shiny::tags$p(
          "No target data selected."))
      }
      if(is.null(tec_embeddings_for_training())==TRUE){
        error_list[length(error_list)+1]=list(shiny::tags$p(
          "No input data selected. Please select a file with document embeddings."))
      }
      if(dir.exists(input$tec_create_select_destination_folder_path)==FALSE){
        error_list[length(error_list)+1]=list(shiny::tags$p(
          "The target directory does not exist. Please check path."))
      }
      if(is.null(input$tec_create_folder_name)|input$tec_create_folder_name==""){
        error_list[length(error_list)+1]=list(shiny::tags$p(
          "Folder name is not set."))
      }
      if(is.null(input$tec_name)|input$tec_create_folder_name==""){
        error_list[length(error_list)+1]=list(shiny::tags$p(
          "Name of the classifier ist not set."))
      }
      if(is.null(input$tec_label)|input$tec_create_folder_name==""){
        error_list[length(error_list)+1]=list(shiny::tags$p(
          "Label of the classifier ist not set."))
      }

      if(length(error_list)>0){
        shinyWidgets::show_alert(title = "Error",
                   text = shiny::tagList(error_list),
                   type = "error")
      } else {
        shiny::showModal(progress_modal)
        destination_path=input$tec_create_select_destination_folder_path

        #if(dir.exists(destination_path)==FALSE){
        #  dir.create(destination_path)
        #}
        target_data=as.factor(tec_target_data_for_training()[[input$tec_target_data_column]])
        names(target_data)=rownames(tec_target_data_for_training())
        if(is.null(input$tec_self_attention_heads)){
          self_attention_heads=0
        } else {
          self_attention_heads=input$tec_self_attention_heads
        }

        if(is.null(input$tec_hidden)|input$tec_hidden==""){
          hidden=NULL
        } else {
          #hidden=as.numeric(stringi::stri_split_regex(input$tec_hidden,pattern=",|[:blank:]")[[1]])
          hidden=as.numeric(stringr::str_extract_all(input$tec_hidden,pattern = "\\d+")[[1]])
        }
        if(is.null(input$tec_rec)|input$tec_rec==""){
          rec=NULL
        } else {
          #rec=as.numeric(stringi::stri_split_regex(input$tec_rec,pattern=",|[:blank:]")[[1]])
          rec=as.numeric(stringr::str_extract_all(input$tec_rec,pattern = "\\d+")[[1]])
        }

        if(is.null(input$tec_bpl_min)){
          bpl_min=0
        } else {
          bpl_min=input$tec_bpl_min
        }

        if(is.null(input$tec_bpl_max)){
          bpl_max=1
        } else {
          bpl_max=input$tec_bpl_max
        }

        withCallingHandlers({
          #Clear Log Output
          log(rep(x="",times=15))
          shinyjs::html(id="pgr_text_output_aifeducation",html = "")

          new_classifier=TextEmbeddingClassifierNeuralNet$new(
            ml_framework=input$config_ml_framework,
            name=input$tec_name,
            label=input$tec_label,
            text_embeddings=tec_embeddings_for_training(),
            targets=target_data,
            hidden=hidden,
            rec=rec,
            self_attention_heads=self_attention_heads,
            intermediate_size=input$tec_intermediate_size,
            attention_type=input$tec_attention_type,
            add_pos_embedding=input$tec_add_pos_embedding,
            rec_dropout=input$tec_rec_dropout,
            repeat_encoder=input$tec_repeat_encoder,
            dense_dropout=input$tec_dense_dropout,
            recurrent_dropout=0.0,
            encoder_dropout=input$tec_encoder_dropout,
            optimizer=input$tec_optimizer)

          new_classifier$train(
            data_embeddings=tec_embeddings_for_training(),
            data_targets=target_data,
            data_n_test_samples=input$tec_data_n_test_samples,
            balance_class_weights=input$tec_balance_class_weights,
            use_baseline=input$tec_use_baseline,
            bsl_val_size=input$tec_bsl_val_size,
            use_bsc=input$tec_use_bsc,
            bsc_methods=input$tec_bsc_methods,
            bsc_max_k=input$tec_bsc_max_k,
            bsc_val_size=input$tec_bsc_val_size,
            bsc_add_all=input$tec_bsc_add_all,
            use_bpl=input$tec_use_bpl,
            bpl_max_steps=input$tec_bpl_max_steps,
            bpl_epochs_per_step=input$tec_epochs,
            bpl_dynamic_inc=input$tec_bpl_dynamic_inc,
            bpl_balance=input$tec_bpl_balance,
            bpl_max=bpl_max,
            bpl_anchor=input$tec_bpl_anchor,
            bpl_min=bpl_min,
            bpl_weight_inc=input$tec_bpl_weight_inc,
            bpl_weight_start=input$tec_bpl_weight_start,
            bpl_model_reset=input$tec_bpl_model_reset,
            sustain_track=input$config_track_sustainability,
            sustain_iso_code=input$config_sustainability_country,
            sustain_region=NULL,
            sustain_interval=15,
            epochs=input$tec_epochs,
            batch_size=input$tec_batch_size,
            dir_checkpoint=input$tec_create_select_destination_folder_path,
            trace=TRUE,
            keras_trace=0,
            pytorch_trace=0,
            n_cores=input$tec_n_cores)
        },
        message=function(m){
          #Vector with old messages
          old_messages<-log()
          new_message=m$message

          #add new message and remove oldes message
          output_print<-c(old_messages[2:length(old_messages)],new_message)

          shinyjs::html(id="pgr_text_output_aifeducation",
                        html = paste(output_print,collapse = "<br>"),
                        add = FALSE)
          #re-assign output
          log(output_print)
        }
        )

        save_ai_model(model = new_classifier,
                      model_dir = destination_path,
                      dir_name = input$tec_create_folder_name)
        shiny::removeModal()
      }

    })

    #Classifier Use Page---------------------------------------------------------
    #Choose Model
    shinyFiles::shinyDirChoose(input=input,
                   id="tec_select_dir_for_use",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = FALSE)
    Classifier_for_Use<-shiny::eventReactive(input$tec_select_dir_for_use,{
      model_path=shinyFiles::parseDirPath(volumes,input$tec_select_dir_for_use)
      if(length(model_path)>0){
        shinyWidgets::show_alert(title="Loading",
                   text = "Please wait",
                   type="info")
        classifier<-try(load_ai_model(model_dir = model_path,
                                      ml_framework=input$config_ml_framework),
                        silent = TRUE)
        if(methods::is(classifier,class2 = "try-error")==FALSE){
          if("TextEmbeddingClassifierNeuralNet"%in%class(classifier)){
            if(utils::compareVersion(as.character(classifier$get_package_versions()$r_package_versions$aifeducation),"0.3.1")>=0){
              shinyWidgets::closeSweetAlert()
              return(classifier)
            } else {
              shinyWidgets::show_alert(title = "Error",
                         text = paste("The classifier was created with aifeducation version",
                                      as.character(classifier$get_package_versions()$r_package_versions$aifeducation,"."),
                                      "Uster interface supports only models created with aifeducation version 0.3.1 or later."),
                         type = "error")
              return(NULL)
            }

          } else {
            shinyWidgets::show_alert(title="Error",
                       text = "The file does not contain an Object of class
                   TextEmbeddingClassifierNeuralNet. Please check the
                   directory.",
                       type="error")
            return(NULL)
          }
        } else {
          shinyWidgets::show_alert(title="Error",
                     text = classifier,
                     type="error")
          return(NULL)
        }
      }
    })

    output$tec_use_selected_model_label<-shiny::renderText({
      if(is.null(Classifier_for_Use())){
        return(NULL)
      } else {
        return (Classifier_for_Use()$get_model_info()$model_label)
      }
    })

    output$tec_use_tabs<-shiny::renderUI({
      if(is.null(Classifier_for_Use())){
        return(NULL)
      } else {
        classifier=Classifier_for_Use()
        measures_shared=intersect(x=names(measure_labels[measures_scale_level]),
                                  y=colnames(classifier$reliability$test_metric_mean))

        reliability_scale=classifier$reliability$test_metric_mean[,measures_shared]
        colnames(reliability_scale)=measure_labels[measures_shared]

        ui<-shinydashboard::box(status = "primary",
                width = 12,
                shiny::tabsetPanel(
                  #Model Description--------------------------------------------------
                  shiny::tabPanel("Model Description",
                           shinydashboard::box(width = 6,
                               status = "primary",
                               shiny::tags$h3(classifier$get_model_info()$model_label),
                               shiny::tags$p("Developers: ",paste(format(x=classifier$get_publication_info()$developed_by$authors,
                                                                  include = c("given","family")),
                                                           collapse = ", ")),
                               shiny::tags$p("Citation: ",classifier$pub_info$developed_by$citation),
                               shiny::tags$p("Date of Creation: ",classifier$get_model_info()$model_date),
                               shiny::tags$p("Software License: ",classifier$get_software_license()),
                               shiny::tags$p("Documentation License: ",classifier$get_documentation_license()),
                               shiny::tags$p("Categories: ",paste(classifier$model_config$target_levels,collapse = ", "))
                           ),
                           shinydashboard::box(width = 6,
                               status = "primary",
                               shiny::tags$h3("Underlying Text Embedding Model"),
                               shiny::tags$p("Label: ",classifier$get_text_embedding_model()$model$model_label),
                               shiny::tags$p("Method: ",classifier$get_text_embedding_model()$model$model_method),
                               shiny::tags$p("Max Tokens Per Chunk: ",classifier$get_text_embedding_model()$model$param_seq_length),
                               shiny::tags$p("Max Chunks: ",classifier$get_text_embedding_model()$model$param_chunks),
                               shiny::tags$p("Token Overlap: ",classifier$get_text_embedding_model()$model$param_overlap),
                               shiny::tags$p("Max Tokens: ",(classifier$get_text_embedding_model()$model$param_seq_length-classifier$get_text_embedding_model()$model$param_overlap)
                                      *classifier$get_text_embedding_model()$model$param_chunks+classifier$get_text_embedding_model()$model$param_seq_length),
                               shiny::tags$p("Hidden States Aggregation: ",classifier$get_text_embedding_model()$model$param_aggregation),
                               shiny::tags$h3("Sustainability"),
                               if(methods::isClass(Class="list",where = classifier$get_sustainability_data())){
                                 if(classifier$get_sustainability_data()$sustainability_tracked==TRUE){
                                   shiny::tags$p("Energy Consumption (kWh): ",classifier$get_sustainability_data()$sustainability_data$total_energy_kwh)
                                 } else {
                                   shiny::tags$p("Energy Consumption (kWh): ","not estimated")
                                 }
                               } else {
                                 shiny::tags$p("Energy Consumption (kWh): ","not estimated")
                               },
                               if(methods::isClass(Class="list",where = classifier$get_sustainability_data())){
                                 if(classifier$get_sustainability_data()$sustainability_tracked==TRUE){
                                   shiny::tags$p("Carbon Footprint (CO2eq. kg): ",classifier$get_sustainability_data()$sustainability_data$co2eq_kg)
                                 } else {
                                   shiny::tags$p("Carbon Footprint (CO2eq. kg): ","not estimated")
                                 }
                               } else {
                                 shiny::tags$p("Carbon Footprint (CO2eq. kg): ","not estimated")
                               }
                           ),
                           shinydashboard::box(width = 12,
                               status = "primary",
                               shinyWidgets::switchInput(
                                 inputId = "tec_desc_language_select",
                                 label = "Language",
                                 onLabel="English",
                                 offLabel = "Native",
                                 value = TRUE,
                                 labelWidth = "80px"),
                               shiny::uiOutput(outputId = "tect_desc_abstract_and_desc")
                           )
                  ),
                  #Training Page------------------------------------------------
                  shiny::tabPanel("Training",
                           shinydashboard::box(title = "Training",
                               solidHeader = TRUE,
                               status = "primary",
                               width = 12,
                               shiny::sidebarLayout(
                                 position="right",
                                 sidebarPanel=shiny::sidebarPanel(
                                   shiny::sliderInput(inputId = "tec_performance_text_size",
                                               label = "Text Size",
                                               min = 1,
                                               max = 20,
                                               step = 0.5,
                                               value = 12),
                                   shiny::numericInput(inputId = "tec_performance_y_min",
                                                label = "Y Min",
                                                value = 0),
                                   shiny::numericInput(inputId = "tec_performance_y_max",
                                                label = "Y Max",
                                                value = 1),
                                   shinyWidgets::radioGroupButtons(
                                     inputId = "tec_performance_training_phase",
                                     label = "Training Phase",
                                     choices = list("Summary Folds"="summary_folds",
                                                    "Final Training"="final_training")),
                                   shinyWidgets::radioGroupButtons(
                                     inputId = "tec_performance_training_measures",
                                     label = "Measures",
                                     choices = list("Loss"="loss",
                                                    "Accuracy"="acc",
                                                    "Balanced Accuracy"="bacc")),
                                   shinyWidgets::materialSwitch(inputId = "tec_performance_training_min_max",
                                                  label = "Add Min/Max",
                                                  value = TRUE,
                                                  status = "primary"),
                                   shiny::uiOutput(outputId = "tec_performance_techniques_widget"),
                                   shiny::uiOutput(outputId = "tec_performance_bpl_steps")
                                 ),
                                 mainPanel =shiny::mainPanel(
                                   shiny::plotOutput(outputId = "tec_performance_training_loss")
                                 )
                               )
                           )
                  ),
                  shiny::tabPanel(title = "Reliability",
                           shinydashboard::box(title = "Coding Stream Analysis",
                               width = 12,
                               status = "primary",
                               solidHeader = TRUE,
                               shiny::sidebarLayout(
                                 position="right",
                                 sidebarPanel = shiny::sidebarPanel(
                                   shiny::sliderInput(inputId = "tec_performance_codings_stream_text_size",
                                               label = "Text Size",
                                               min = 1,
                                               max = 20,
                                               value = 10,
                                               step = 0.25),
                                   shiny::sliderInput(inputId = "tec_performance_codings_stream_labels_size",
                                               label = "Text Size Categories",
                                               min = 0.1,
                                               max = 5,
                                               value = 3,
                                               step = 0.1),
                                   shiny::sliderInput(inputId = "tec_performance_codings_stream_key_size",
                                               label = "Key Size",
                                               min = 0.1,
                                               max = 2,
                                               value = 0.1,
                                               step = 0.1)
                                 ),
                                 mainPanel =shiny::mainPanel(
                                   shiny::plotOutput(outputId = "tec_performance_coding_stream_plot"),
                                   shiny::tags$p("Note: Plot is calculated based on a freely estimated Assignment-Error-Matrix.
                                          The categorical sizes are based on the relative frequencies of the training data.
                                          These sizes are not identical with the sizes of field samples.")
                                 )
                               )
                           ),
                           shinydashboard::box(title = "Spectral Analysis",
                               width = 12,
                               status = "primary",
                               solidHeader = TRUE,
                               shiny::sidebarLayout(
                                 position="right",
                                 sidebarPanel = shiny::sidebarPanel(
                                   shiny::sliderInput(inputId = "tec_performance_codings_spectral_text_size",
                                               label = "Text Size",
                                               min = 1,
                                               max = 20,
                                               value = 10,
                                               step = 0.25),
                                   shiny::sliderInput(inputId = "tec_performance_codings_spectral_number_size",
                                               label = "Number Size",
                                               min = 0.1,
                                               max = 5,
                                               value = 3,
                                               step = 0.1),
                                   shiny::sliderInput(inputId = "tec_performance_codings_spectral_key_size",
                                               label = "Key Size",
                                               min = 0.1,
                                               max = 2,
                                               value = 0.1,
                                               step = 0.1)
                                 ),
                                 mainPanel =shiny::mainPanel(
                                   shiny::plotOutput(outputId = "tec_performance_coding_spectral_plot"),
                                   shiny::tags$p("Note: Plot is calculated based on a freely estimated Assignment-Error-Matrix.
                                          The categorical sizes are based on the relative frequencies of the training data.
                                          These sizes are not identical with the sizes of field samples.")
                                 )
                               )
                           ),
                           shinydashboard::box(title="Measures",
                               solidHeader = TRUE,
                               status = "primary",
                               width = 12,
                               shinydashboard::box(title = "Scale Level",
                                   status = NULL,
                                   solidHeader = TRUE,
                                   width = 6,
                                   shiny::renderTable(t(reliability_scale),
                                               rownames=TRUE,
                                               colnames = TRUE),
                                   shiny::tags$p("Note: Values for Dynamic Iota Index are calculated based on a restricted Assignment-Error-Matrix.")
                               ),
                               shinydashboard::box(title = "Categorical Level",
                                   status = NULL,
                                   solidHeader = TRUE,
                                   width = 6,
                                   shiny::tags$p(shiny::tags$b("Assignment-Error-Matrix")),
                                   shiny::renderTable(classifier$reliability$iota_object_end_free$categorical_level$raw_estimates$assignment_error_matrix,
                                               rownames = TRUE,
                                               colnames = TRUE),
                                   shiny::tags$p(shiny::tags$b("Iota")),
                                   shiny::renderTable(t(as.matrix(classifier$reliability$iota_object_end_free$categorical_level$raw_estimates$iota))),
                                   shiny::tags$p(shiny::tags$b("Alpha Reliability")),
                                   shiny::renderTable(t(as.matrix(classifier$reliability$iota_object_end_free$categorical_level$raw_estimates$alpha_reliability))),
                                   shiny::tags$p(shiny::tags$b("Beta Reliability")),
                                   shiny::renderTable(t(as.matrix(classifier$reliability$iota_object_end_free$categorical_level$raw_estimates$beta_reliability))),
                                   shiny::tags$p("Note: All values are calculated based on a freely estimated Assignment-Error-Matrix.")

                               )
                           )
                  ),
                  #Prediction Page--------------------------------------------------
                  shiny::tabPanel("Prediction",
                           shinydashboard::box(title = "Text Embeddings",
                               solidHeader = TRUE,
                               status = "primary",
                               shinyFiles::shinyFilesButton(id="tec_select_embeddings_for_prediction",
                                                label="Choose file",
                                                title="Please choose a file",
                                                icon=shiny::icon("file"),
                                                multiple=FALSE),
                               shiny::uiOutput(outputId = "tec_embeddings_for_prediction")
                           ),
                           shinydashboard::box(title = "Prediction Destination",
                               solidHeader = TRUE,
                               status = "primary",
                               shinyFiles::shinyDirButton(id="tec_choose_file_path_for_predictions",
                                              title="Choose a directory for storing the predictions",
                                              label = "Select Folder",
                                              icon=shiny::icon("folder-open")),
                               shiny::textInput(inputId = "tec_choose_file_path_for_predictions_path",
                                         label = shiny::tags$p(shiny::icon("file"),"Path to Folder")),
                               shiny::textInput(inputId = "tec_choose_file_path_for_predictions_file_name",
                                         label = shiny::tags$p(shiny::icon("file"),"File Name")),
                               shiny::sliderInput(inputId = "tec_predict_batch_size",
                                           label = "Batch size",
                                           min = 1,
                                           max = 256,
                                           value = 8),
                               shiny::tags$p(shiny::tags$b("Save Formats")),
                               shinyWidgets::materialSwitch(inputId = "tec_pred_save_as_rda",
                                              label = "Save as .rda ",
                                              right = TRUE,
                                              inline = TRUE,
                                              value = TRUE,
                                              status = "primary"),
                               shinyWidgets::materialSwitch(inputId = "tec_pred_save_as_csv",
                                              label = "Save as .csv ",
                                              right = TRUE,
                                              inline = TRUE,
                                              value = TRUE,
                                              status = "primary"),
                               shiny::actionButton(inputId = "text_start_prediction",
                                            label = "Start prediction",
                                            icon = shiny::icon("paper-plane"))
                           )
                  )
                )
        )
        return(ui)
      }
    })

    output$tec_performance_coding_stream_plot<-shiny::renderPlot(expr={
      if(is.null(Classifier_for_Use())){
        return(NULL)
      } else {
        classifier=Classifier_for_Use()
        plot<-iotarelr::plot_iota2_alluvial(
          object = classifier$reliability$iota_object_end_free,
          label_categories_size = input$tec_performance_codings_stream_labels_size,
          key_size=input$tec_performance_codings_stream_key_size,
          text_size = input$tec_performance_codings_stream_text_size)
        return(plot)
      }
    },
    res = 2*72)

    output$tec_performance_bpl_steps<-shiny::renderUI({
      if(!is.null(input$tec_performance_training_techniques)){
        if(input$tec_performance_training_techniques=="bpl"){
          classifier=classifier=Classifier_for_Use()
          n_steps=ncol(classifier$last_training$data_pbl)
          return(
            shinyWidgets::radioGroupButtons(
              inputId = "tec_training_bpl_step",
              label = "Step during Pseudo Labeling",
              choices = seq.int(from = 1,to=n_steps))
          )
        } else {
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })

    output$tec_performance_coding_stream_plot<-shiny::renderPlot(expr={
      if(is.null(Classifier_for_Use())){
        return(NULL)
      } else {
        classifier=Classifier_for_Use()
        plot<-iotarelr::plot_iota2_alluvial(
          object = classifier$reliability$iota_object_end_free,
          label_categories_size = input$tec_performance_codings_stream_labels_size,
          key_size=input$tec_performance_codings_stream_key_size,
          text_size = input$tec_performance_codings_stream_text_size)
        return(plot)
      }
    },
    res = 2*72)

    output$tec_performance_coding_spectral_plot<-shiny::renderPlot(expr={
      if(is.null(Classifier_for_Use())){
        return(NULL)
      } else {
        classifier=Classifier_for_Use()
        plot<-iotarelr::plot_iota(
          object = classifier$reliability$iota_object_end_free,
          number_size = input$tec_performance_codings_spectral_number_size,
          key_size=input$tec_performance_codings_spectral_key_size,
          text_size = input$tec_performance_codings_spectral_text_size)
        return(plot)
      }
    },
    res = 2*72)



    #Documentation
    output$tect_desc_abstract_and_desc<-shiny::renderUI({
      classifier=Classifier_for_Use()
      if(!is.null(classifier)){
        language_eng=input$tec_desc_language_select
        if(language_eng==TRUE){
          ui<-list(
            shiny::tags$h3("Abstract"),
            if(!is.null(classifier$get_model_description()$abstract_eng)){
              shiny::tags$p(shiny::includeMarkdown(classifier$get_model_description()$abstract_eng))
            },
            shiny::tags$h3("Description"),
            if(!is.null(classifier$get_model_description()$eng)){
              shiny::tags$p(shiny::includeMarkdown(classifier$get_model_description()$eng))
            }
          )
        } else {
          ui<-list(
            shiny::tags$h3("Abstract"),
            if(!is.null(classifier$get_model_description()$abstract_native)){
              shiny::tags$p(shiny::includeMarkdown(classifier$get_model_description()$abstract_native))
            },
            shiny::tags$h3("Description"),
            if(!is.null(classifier$get_model_description()$native)){
              shiny::tags$p(shiny::includeMarkdown(classifier$get_model_description()$native))
            }
          )
        }
        return(ui)
      } else {
        return(NULL)
      }
    })

    #Performance
    output$tec_performance_techniques_widget<-shiny::renderUI({
      classifier=Classifier_for_Use()

      if(!is.null(classifier) & !is.null(input$tec_performance_training_phase)){
        training_techniques=NULL
        if(classifier$last_training$config$use_baseline==TRUE &
           input$tec_performance_training_phase!="final_training"){
          training_techniques["Baseline Model"]="bsl"
        }
        if(classifier$last_training$config$use_bsc==TRUE){
          training_techniques["Balanced Synthetic Cases"]="bsc"
        }
        if(classifier$last_training$config$use_bpl==TRUE){
          training_techniques["Balanced Pseudo Labeling"]="bpl"
        }
        return(shinyWidgets::radioGroupButtons(
          inputId = "tec_performance_training_techniques",
          label = "Techniques",
          choices = training_techniques)
        )
      } else {
        return(NULL)
      }

    })


    performance_data_for_visual<-shiny::reactive({
      if(is.null(Classifier_for_Use())==FALSE &
         is.null(input$tec_performance_training_techniques)==FALSE &
         is.null(input$tec_performance_training_phase)==FALSE){

        if(input$tec_performance_training_techniques=="bsl" &
           input$tec_performance_training_phase=="final_training"){
          return(NULL)
        } else {
          classifier=Classifier_for_Use()
          plot_data=classifier$last_training$history[[input$tec_performance_training_techniques]]

          if(input$tec_performance_training_phase=="summary_folds"){
            if(input$tec_performance_training_techniques!="bpl"){
              n_epochs=ncol(plot_data[[1]]$loss)
              n_sample_type=nrow(plot_data[[1]]$loss)
              n_folds=classifier$last_training$n_samples
            } else {
              n_epochs=ncol(plot_data[[1]][[as.numeric(input$tec_training_bpl_step)]]$loss)
              n_sample_type=nrow(plot_data[[1]][[as.numeric(input$tec_training_bpl_step)]]$loss)
              n_folds=classifier$last_training$n_samples
            }
          } else {
            if(input$tec_performance_training_techniques!="bpl"){
              n_epochs=ncol(plot_data[["final"]]$loss)
              n_sample_type=nrow(plot_data[["final"]]$loss)
              n_folds=1
            } else {
              n_epochs=ncol(plot_data[["final"]][[as.numeric(input$tec_training_bpl_step)]]$loss)
              n_sample_type=nrow(plot_data[["final"]][[as.numeric(input$tec_training_bpl_step)]]$loss)
              n_folds=1
            }
          }

          if(n_sample_type==3){
            sample_type_name=c("train","validation","test")
          } else {
            sample_type_name=c("train","validation")
          }

          loss_array=array(dim = c(n_folds,
                                   n_sample_type,
                                   n_epochs),
                           dimnames = list(fold=NULL,sample_type=sample_type_name,epoch=NULL))
          bacc_array=loss_array
          acc_array=loss_array

          final_data_loss=matrix(data = NA,
                                 nrow = n_epochs,
                                 ncol = 3*n_sample_type+1)
          colnames(final_data_loss)=c("epoch",
                                      paste0(sample_type_name,
                                             c(rep("_min",times=n_sample_type),
                                               rep("_mean",times=n_sample_type),
                                               rep("_max",times=n_sample_type))))
          final_data_loss[,"epoch"]=seq.int(from = 1,to=n_epochs)
          final_data_bacc=final_data_loss
          final_data_acc=final_data_loss

          if(input$tec_performance_training_phase=="summary_folds"){
            for(i in 1:n_folds){
              if(input$tec_performance_training_techniques!="bpl"){
                loss_array[i,,]=plot_data[[i]]$loss
                bacc_array[i,,]=plot_data[[i]]$balanced_accuracy
                acc_array[i,,]=plot_data[[i]]$accuracy
              } else {
                loss_array[i,,]=plot_data[[i]][[as.numeric(input$tec_training_bpl_step)]]$loss
                bacc_array[i,,]=plot_data[[i]][[as.numeric(input$tec_training_bpl_step)]]$balanced_accuracy
                acc_array[i,,]=plot_data[[i]][[as.numeric(input$tec_training_bpl_step)]]$accuracy
              }
            }
          } else if(input$tec_performance_training_phase=="final_training"){
            if(input$tec_performance_training_techniques!="bpl"){
              loss_array[1,,]=plot_data[["final"]]$loss
              bacc_array[1,,]=plot_data[["final"]]$balanced_accuracy
              acc_array[1,,]=plot_data[["final"]]$accuracy
            } else {
              loss_array[1,,]=plot_data[["final"]][[as.numeric(input$tec_training_bpl_step)]]$loss
              bacc_array[1,,]=plot_data[["final"]][[as.numeric(input$tec_training_bpl_step)]]$balanced_accuracy
              acc_array[1,,]=plot_data[["final"]][[as.numeric(input$tec_training_bpl_step)]]$accuracy
            }
          }

          for(i in 1:n_epochs){
            final_data_loss[i,"train_min"]=min(loss_array[,"train",i])
            final_data_loss[i,"train_mean"]=mean(loss_array[,"train",i])
            final_data_loss[i,"train_max"]=max(loss_array[,"train",i])

            final_data_bacc[i,"train_min"]=min(bacc_array[,"train",i])
            final_data_bacc[i,"train_mean"]=mean(bacc_array[,"train",i])
            final_data_bacc[i,"train_max"]=max(bacc_array[,"train",i])

            final_data_acc[i,"train_min"]=min(acc_array[,"train",i])
            final_data_acc[i,"train_mean"]=mean(acc_array[,"train",i])
            final_data_acc[i,"train_max"]=max(acc_array[,"train",i])

            final_data_loss[i,"validation_min"]=min(loss_array[,"validation",i])
            final_data_loss[i,"validation_mean"]=mean(loss_array[,"validation",i])
            final_data_loss[i,"validation_max"]=max(loss_array[,"validation",i])

            final_data_bacc[i,"validation_min"]=min(bacc_array[,"validation",i])
            final_data_bacc[i,"validation_mean"]=mean(bacc_array[,"validation",i])
            final_data_bacc[i,"validation_max"]=max(bacc_array[,"validation",i])

            final_data_acc[i,"validation_min"]=min(acc_array[,"validation",i])
            final_data_acc[i,"validation_mean"]=mean(acc_array[,"validation",i])
            final_data_acc[i,"validation_max"]=max(acc_array[,"validation",i])
            if(n_sample_type==3){
              final_data_loss[i,"test_min"]=min(loss_array[,"test",i])
              final_data_loss[i,"test_mean"]=mean(loss_array[,"test",i])
              final_data_loss[i,"test_max"]=max(loss_array[,"test",i])

              final_data_bacc[i,"test_min"]=min(bacc_array[,"test",i])
              final_data_bacc[i,"test_mean"]=mean(bacc_array[,"test",i])
              final_data_bacc[i,"test_max"]=max(bacc_array[,"test",i])

              final_data_acc[i,"test_min"]=min(acc_array[,"test",i])
              final_data_acc[i,"test_mean"]=mean(acc_array[,"test",i])
              final_data_acc[i,"test_max"]=max(acc_array[,"test",i])
            }
          }
          return(list(loss=as.data.frame(final_data_loss),
                      bacc=as.data.frame(final_data_bacc),
                      acc=as.data.frame(final_data_acc)))
        }
      } else {
        return(NULL)
      }
    })

    output$tec_performance_training_loss<-shiny::renderPlot({
      plot_data=performance_data_for_visual()[[input$tec_performance_training_measures]]

      if(!is.null(plot_data)){
        y_min=input$tec_performance_y_min
        y_max=input$tec_performance_y_max
        if(input$tec_performance_training_measures=="loss"){
          y_label="loss"
        } else if(input$tec_performance_training_measures=="acc"){
          y_label="Accuracy"
        } else if(input$tec_performance_training_measures=="bacc"){
          y_label="Balanced Accuracy"
        }

        plot<-ggplot2::ggplot(data=plot_data)+
          ggplot2::geom_line(ggplot2::aes(x=rlang::.data$epoch,y=rlang::.data$train_mean,color="train"))+
          ggplot2::geom_line(ggplot2::aes(x=rlang::.data$epoch,y=rlang::.data$validation_mean,color="validation"))

        if(input$tec_performance_training_min_max==TRUE){
          plot<-plot+
            ggplot2::geom_line(ggplot2::aes(x=rlang::.data$epoch,y=rlang::.data$train_min,color="train"))+
            ggplot2::geom_line(ggplot2::aes(x=rlang::.data$epoch,y=rlang::.data$train_max,color="train"))+
            ggplot2::geom_ribbon(ggplot2::aes(x=rlang::.data$epoch,
                                              ymin=rlang::.data$train_min,
                                              ymax=rlang::.data$train_max),
                                 alpha=0.25,
                                 fill="red")+
            ggplot2::geom_line(ggplot2::aes(x=rlang::.data$epoch,y=rlang::.data$validation_min,color="validation"))+
            ggplot2::geom_line(ggplot2::aes(x=rlang::.data$epoch,y=rlang::.data$validation_max,color="validation"))+
            ggplot2::geom_ribbon(ggplot2::aes(x=rlang::.data$epoch,
                                              ymin=rlang::.data$validation_min,
                                              ymax=rlang::.data$validation_max),
                                 alpha=0.25,
                                 fill="blue")
        }
        if("test_mean"%in%colnames(plot_data)){
          plot=plot+
            ggplot2::geom_line(ggplot2::aes(x=rlang::.data$epoch,y=rlang::.data$test_mean,color="test"))
          if(input$tec_performance_training_min_max==TRUE){
            plot=plot+
              ggplot2::geom_line(ggplot2::aes(x=rlang::.data$epoch,y=rlang::.data$test_min,color="test"))+

              ggplot2::geom_line(ggplot2::aes(x=rlang::.data$epoch,y=rlang::.data$test_max,color="test"))+
              ggplot2::geom_ribbon(ggplot2::aes(x=rlang::.data$epoch,
                                                ymin=rlang::.data$test_min,
                                                ymax=rlang::.data$test_max),
                                   alpha=0.25,
                                   fill="darkgreen")
          }
        }

        plot=plot+ggplot2::theme_classic()+
          ggplot2::ylab(y_label)+
          ggplot2::coord_cartesian(ylim=c(y_min,y_max))+
          ggplot2::xlab("epoch")+
          ggplot2::scale_color_manual(values = c("train"="red",
                                                 "validation"="blue",
                                                 "test"="darkgreen"))+
          ggplot2::theme(text = ggplot2::element_text(size = input$tec_performance_text_size),
                         legend.position="bottom")
        return(plot)
      } else {
        return(NULL)
      }
    },res = 72*2)

    #Prediction Page--------------------------------------------------------------
    shinyFiles::shinyFileChoose(input=input,
                    id="tec_select_embeddings_for_prediction",
                    roots = volumes,
                    filetype=c("rda","rdata"))

    tec_embeddings_for_prediction_path=shiny::eventReactive(input$tec_select_embeddings_for_prediction,{
      tmp_file_path=shinyFiles::parseFilePaths(volumes,input$tec_select_embeddings_for_prediction)
      if(nrow(tmp_file_path)>0){
        return(tmp_file_path[[1,"datapath"]])
      } else {
        return(NULL)
      }
    })

    tec_embeddings_for_prediction=shiny::eventReactive(tec_embeddings_for_prediction_path(),{
      file_path=tec_embeddings_for_prediction_path()
      if(!is.null(file_path)){
        if(file.exists(file_path)==TRUE){
          shinyWidgets::show_alert(title="Loading",
                     text = "Please wait",
                     type="info")
          file=load(file_path)
          embeddings=get(x=file)
          if(("EmbeddedText" %in% class(embeddings))==TRUE){
            classifier=Classifier_for_Use()
            if(classifier$check_embedding_model(embeddings)==FALSE){
              shinyWidgets::closeSweetAlert()
              shinyWidgets::show_alert(title="Error",
                         text = "The TextEmbeddingModel of the text embeddings
                       and the classifier are not the same.",
                         type="error")
              rm(embeddings)
              gc()
              return(NULL)
            } else {
              shinyWidgets::closeSweetAlert()
              return(embeddings)
            }
          } else {
            shinyWidgets::closeSweetAlert()
            shinyWidgets::show_alert(title="Error",
                       text = "The file contains data in an unsupported format.
                     Text embeddings must be of class 'EmbeddedText'. Please
                     check data. Data embeddings should always be created via data
                     preparation of this user interfache or with the corresponding
                     method of the TextEmbeddingModel.",
                       type="error")
            rm(embeddings)
            gc()
            return(NULL)
          }
        } else {
          shinyWidgets::closeSweetAlert()
          shinyWidgets::show_alert(title="Error",
                     text = "The file does not exist on the path.",
                     type="error")
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })

    output$tec_embeddings_for_prediction<-shiny::renderUI({
      embeddings=tec_embeddings_for_prediction()
      if(!is.null(embeddings)){
        model_info=embeddings$get_model_info()
        info_table=matrix(nrow = 3,
                          ncol = 4,
                          data = "")
        info_table[1,1]="Model Method:"
        info_table[2,1]="Hidden State Aggregation:"
        info_table[3,1]="Model Language:"

        info_table[1,2]=model_info$model_method
        info_table[2,2]=model_info$param_aggregation
        info_table[3,2]=model_info$model_language

        info_table[1,3]="Tokens per Chunk:"
        info_table[2,3]="Max Chunks:"
        info_table[3,3]="Token Overlap:"

        info_table[1,4]=model_info$param_seq_length
        info_table[2,4]=model_info$param_chunks
        info_table[3,4]=model_info$param_overlap

        ui<-list(
          shinydashboard::valueBox(value=nrow(embeddings$embeddings),
                   subtitle="Number of Cases",
                   icon = shiny::icon("list"),
                   width=12),
          shiny::tags$h3("Model:",  model_info$model_label),
          shiny::tags$p("Model Name:", model_info$model_name),
          shiny::tags$p("Created", model_info$model_date),
          shiny::renderTable(expr=info_table,
                      colnames=FALSE)
        )
        return(ui)
      } else {
        return(NULL)
      }
    })

    #Destination folder for prediction
    shinyFiles::shinyDirChoose(input=input,
                   id="tec_choose_file_path_for_predictions",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = TRUE)
    shiny::observeEvent(input$tec_choose_file_path_for_predictions,{
      shiny::updateTextInput(inputId = "tec_choose_file_path_for_predictions_path",
                      value = shinyFiles::parseDirPath(volumes,input$tec_choose_file_path_for_predictions))
    })

    #Start Prediction
    shiny::observeEvent(input$text_start_prediction,{
      dir_path=input$tec_choose_file_path_for_predictions_path

      error_list=NULL
      #Check if all inputs are correctly set
      if(dir.exists(dir_path)==FALSE){
        error_list[length(error_list)+1]=list(
          shiny::tags$p("Source directory does not exist. Please check your directory path."))
      }
      if(is.null(tec_embeddings_for_prediction())){
        error_list[length(error_list)+1]=list(
          shiny::tags$p("No text embeddings provided. Please provide compatibel embeddings."))
      }
      if(input$tec_pred_save_as_rda==FALSE &
         input$tec_pred_save_as_csv==FALSE){
        error_list[length(error_list)+1]=list(
          shiny::tags$p("No save format selected. At least one save format must be selected."))
      }
      if(is.null(input$tec_choose_file_path_for_predictions_file_name) |
         input$tec_choose_file_path_for_predictions_file_name==""){
        error_list[length(error_list)+1]=list(
          shiny::tags$p("No file name provided for saving the predictions."))
      }

      if(length(error_list)>0){
        shinyWidgets::show_alert(title = "Error",
                   text = shiny::tagList(error_list),
                   type = "error")
      } else {
        shiny::showModal(progress_modal)
        save_path_root=paste0(dir_path,"/",input$tec_choose_file_path_for_predictions_file_name)
        classifier=Classifier_for_Use()
        predictions=classifier$predict(
          newdata=tec_embeddings_for_prediction(),
          batch_size=input$tec_predict_batch_size,
          verbose=0)

        if(input$tec_pred_save_as_rda==TRUE){
          save(predictions,
               file = paste0(save_path_root,".rda"))
        }
        if(input$tec_pred_save_as_csv==TRUE){
          utils::write.csv2(predictions,
                     file = paste0(save_path_root,".csv"))
        }
        shiny::removeModal()
      }

    })

    #Documentation Page----------------------------------------------------------------
    shinyFiles::shinyDirChoose(input=input,
                   id="tec_db_select_model_for_documentation",
                   roots = volumes,
                   #session = session,
                   allowDirCreate = FALSE)
    tec_interface_for_documentation_path=shiny::eventReactive(input$tec_db_select_model_for_documentation,{
      return(shinyFiles::parseDirPath(volumes,input$tec_db_select_model_for_documentation))
    })

    Classifier_for_Documentation<-shiny::eventReactive(tec_interface_for_documentation_path(),{
      if(length(tec_interface_for_documentation_path())>0){
        shinyWidgets::show_alert(title="Working",
                   text = "Please wait",
                   type="info")
        classifier=try(load_ai_model(model_dir = tec_interface_for_documentation_path(),
                                     ml_framework=input$config_ml_framework),
                       silent = TRUE)
        if(methods::is(classifier,class2 = "try-error")==FALSE){
          if("TextEmbeddingClassifierNeuralNet"%in%class(classifier)){
            if(utils::compareVersion(as.character(classifier$get_package_versions()$r_package_versions$aifeducation),"0.3.1")>=0){
              shinyWidgets::closeSweetAlert()
              return(classifier)
            } else {
              shinyWidgets::show_alert(title = "Error",
                         text = paste("The classifier was created with aifeducation version",
                                      as.character(classifier$get_package_versions()$r_package_versions$aifeducation,"."),
                                      "Uster interface supports only models created with aifeducation version 0.3.1 or later."),
                         type = "error")
              return(NULL)
            }
          } else {
            shinyWidgets::show_alert(title = "Error",
                       text = "The directory does not contain an object of class TextEmbeddingClassifierNeuralNet.
                   Please check your directory.",
                       type = "error")
            return(NULL)
          }
        } else {
          shinyWidgets::show_alert(title = "Error",
                     text = classifier,
                     type = "error")
          return(NULL)
        }
      } else {
        return(NULL)
      }
    })

    output$tec_document_selected_model_label<-shiny::renderText({
      if(is.null(Classifier_for_Documentation())){
        return(NULL)
      } else {
        return (Classifier_for_Documentation()$get_model_info()$model_label)
      }
    })

    output$tec_document_tabs<-shiny::renderUI({
      classifier<-Classifier_for_Documentation()
      if(is.null(classifier)){
        return(NULL)
      } else {

        ui_parts=NULL
        pup_info_for=c("developed_by")
        pup_info_titles=c("Developers")
        for(i in 1:length(pup_info_for)){
          widgets=NULL
          for(j in 1:10){
            pup_info=classifier$get_publication_info()[[pup_info_for[i]]]$authors
            widgets[[j]]=list(
              shiny::fluidRow(
                shiny::column(width = 4,
                       shiny::textInput(inputId = paste0("tec_doc_",pup_info_titles[i],"_fist_name_",j),
                                 label = paste("Given Name",j),
                                 value = pup_info[[j]]$given,
                                 width = "100%")
                ),
                shiny::column(width = 4,
                       shiny::textInput(inputId = paste0("tec_doc_",pup_info_titles[i],"_last_name_",j),
                                 label = paste("Family Name",j),
                                 value = pup_info[[j]]$family,
                                 width = "100%")
                ),
                shiny::column(width = 4,
                       shiny::textInput(inputId = paste0("tec_doc_",pup_info_titles[i],"_mail_",j),
                                 label = paste("Mail",j),
                                 value = pup_info[[j]]$email,
                                 width = "100%")
                )
              )
            )
          }
          ui_parts[length(ui_parts)+1]=list(
            shiny::tabPanel(title = pup_info_titles[i],
                     widgets,
                     shiny::textInput(inputId = paste0("tec_doc_",pup_info_for[i],"_citation"),
                               label = "Citation",
                               value = classifier$get_publication_info()[[pup_info_for[i]]]$citation),
                     shiny::textInput(inputId = paste0("tec_doc_",pup_info_for[i],"_url"),
                               label = "URL",
                               value = classifier$get_publication_info()[[pup_info_for[i]]]$url),
                     shiny::actionButton(inputId = paste0("tec_doc_",pup_info_for[i],"_save"),
                                  label = "Save",
                                  icon = shiny::icon("floppy-disk"))
            )
          )
        }

        documention_part=c("abstract_eng","abstract_native","description_eng","description_native")
        documention_titles=c("Abstract English","Abstract Native","Description English","Description Native")
        documentation_field=c("abstract_eng","abstract_native","eng","native")
        documentation_keywords=c("keywords_eng","keywords_native")
        for(i in 1:length(documention_part)){

          tmp_tabPanel<-shiny::tabPanel(
            title = documention_titles[i],
            shiny::fluidRow(
              shiny::column(width = 6,
                     shiny::textAreaInput(inputId = paste0("tec_doc_editor_",documention_part[i]),
                                   label = "Editor",
                                   rows = 6,
                                   value = classifier$get_model_description()[[documentation_field[i]]]
                     ),
                     if(i<=2){
                       shiny::textInput(inputId = paste0("tec_doc_editor_",documention_part[i],"_keywords"),
                                 value = classifier$get_model_description()[[documentation_keywords[i]]],
                                 label = "Keywords")
                     },
                     shiny::actionButton(inputId = paste0("tec_doc_editor_",documention_part[i],"_preview_button"),
                                  label = "Preview",
                                  icon = shiny::icon("eye")),
                     shiny::actionButton(inputId = paste0("tec_doc_editor_",documention_part[i],"_save_button"),
                                  label = "Save",
                                  icon = shiny::icon("floppy-disk"))),
              shiny::column(width = 6,
                     shiny::tags$p(shiny::tags$b("Preview")),
                     shiny::uiOutput(outputId = paste0("tec_doc_editor_",documention_part[i],"_preview")))
            )
          )
          ui_parts[length(ui_parts)+1]=list(tmp_tabPanel)
        }

        ui<-shinydashboard::tabBox(width = 12,
                   ui_parts[[1]],
                   ui_parts[[2]],
                   ui_parts[[3]],
                   ui_parts[[4]],
                   ui_parts[[5]]
        )
        return(ui)
      }

    })

    #Preview Events
    shiny::observeEvent(input$tec_doc_editor_abstract_eng_preview_button,{
      output$tec_doc_editor_abstract_eng_preview<-shiny::renderUI({
        return(shiny::includeMarkdown(input$tec_doc_editor_abstract_eng))
      })
    })
    shiny::observeEvent(input$tec_doc_editor_abstract_native_preview_button,{
      output$tec_doc_editor_abstract_native_preview<-shiny::renderUI({
        return(shiny::includeMarkdown(input$tec_doc_editor_abstract_native))
      })
    })
    shiny::observeEvent(input$tec_doc_editor_description_eng_preview_button,{
      output$tec_doc_editor_description_eng_preview<-shiny::renderUI({
        return(shiny::includeMarkdown(input$tec_doc_editor_description_eng))
      })
    })
    shiny::observeEvent(input$tec_doc_editor_description_native_preview_button,{
      output$tec_doc_editor_description_native_preview<-shiny::renderUI({
        return(shiny::includeMarkdown(input$tec_doc_editor_description_native))
      })
    })


    #Save Events
    #Developers
    shiny::observeEvent(input$tec_doc_developed_by_save,{
      model<-Classifier_for_Documentation()

      tmp_person_list=NULL
      for(i in 1:10){
        given=input[[paste0("tec_doc_","Developers","_fist_name_",i)]]
        family=input[[paste0("tec_doc_","Developers","_last_name_",i)]]
        mail=input[[paste0("tec_doc_","Developers","_mail_",i)]]
        if((!is.null(given) & !(given="")) &
           (!is.null(family) & !(family==""))){
          person=person(given=given,family=family,email=mail)
          tmp_person_list=append(x=tmp_person_list,
                                 values = person)
        }
      }

      model$set_publication_info(authors = tmp_person_list,
                                 citation = input[[paste0("tec_doc_","developed_by","_citation")]],
                                 url = input[[paste0("tec_doc_","developed_by","_url")]])
      r_interface_path=paste0(tec_interface_for_documentation_path(),"/r_interface.rda")
      save(model,file = r_interface_path)
      Classifier_for_Documentation<-shiny::reactive({model})

    })

    #Modifiers

    shiny::observeEvent(input$tec_doc_editor_abstract_eng_save_button,{
      model<-Classifier_for_Documentation()
      model$set_model_description(
        abstract_eng=input$tec_doc_editor_abstract_eng,
        keywords_eng=input$tec_doc_editor_abstract_eng_keywords)
      r_interface_path=paste0(tec_interface_for_documentation_path(),"/r_interface.rda")
      save(model,file = r_interface_path)
      Classifier_for_Documentation<-shiny::reactive({model})
    })
    shiny::observeEvent(input$tec_doc_editor_abstract_native_save_button,{
      model<-Classifier_for_Documentation()
      model$set_model_description(
        abstract_native=input$tec_doc_editor_abstract_native,
        keywords_native=input$tec_doc_editor_abstract_native_keywords)
      r_interface_path=paste0(tec_interface_for_documentation_path(),"/r_interface.rda")
      save(model,file = r_interface_path)
      Classifier_for_Documentation<-shiny::reactive({model})

    })
    shiny::observeEvent(input$tec_doc_editor_description_eng_save_button,{
      model<-Classifier_for_Documentation()
      model$set_model_description(
        eng =input$tec_doc_editor_description_eng)
      r_interface_path=paste0(tec_interface_for_documentation_path(),"/r_interface.rda")
      save(model,file = r_interface_path)
      Classifier_for_Documentation<-shiny::reactive({model})
    })
    shiny::observeEvent(input$tec_doc_editor_description_native_save_button,{
      model<-Classifier_for_Documentation()
      model$set_model_description(
        native =input$tec_doc_editor_description_native)
      r_interface_path=paste0(tec_interface_for_documentation_path(),"/r_interface.rda")
      save(model,file = r_interface_path)
      Classifier_for_Documentation<-shiny::reactive({model})
    })

    #Progress_Modal--------------------------------------------------------------
    progress_modal=shiny::modalDialog(
      title="In progress. Please wait.",
      easyClose = FALSE,
      size = "l",
      shinyWidgets::progressBar(id = "pgr_bar_aifeducation",
                  value = 0,
                  display_pct = TRUE,
                  title=""),
      shinyWidgets::progressBar(id = "pgr_bar_aifeducation_epochs",
                  value = 0,
                  display_pct = TRUE,
                  title="Epoch"),
      shinyWidgets::progressBar(id = "pgr_bar_aifeducation_steps",
                  value = 0,
                  display_pct = TRUE,
                  title="Batche/Step"),
      shiny::tags$p(shiny::tags$b("Log")),
      shiny::uiOutput(outputId = "pgr_text_output_aifeducation"),
      footer="To stop the progress please close the browser."
      #footer=shiny::actionButton(inputId = "pgr_cancel",
      #                    label = "Cancel",
      #                    icon = shiny::icon("ban"))
    )

    #  shiny::observeEvent(input$pgr_cancel,{
    #    shiny::stopApp()
    #  })
  }
  # Run the app ----
  shiny::shinyApp(ui = ui, server = server)
}
