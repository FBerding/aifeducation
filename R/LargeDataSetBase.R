
#'@export
LargeDataSetBase<-R6::R6Class(
  classname = "LargeDataSetBase",
  public = list(
    n_cols=function(){
      return(private$data$num_columns)
    },
    #------------------------------------------------------
    n_rows=function(){
      return(private$data$num_rows)
    },
    #-----------------------------------------------------
    get_colnames=function(){
      return(private$data$column_names)
    },
    #-----------------------------------------------------
    get_dataset=function(){
      return(private$data)
    },
    #------------------------------------------------------
    reduce_to_unique_ids=function(){
      private$data=reduce_to_unique(private$data,"id")
    },
    #----------------------------------------------------
    select=function(indicies){
      private$data$set_format("np")
      if(length(indicies)>1){
        return(private$data$select(as.integer(indicies)))
      } else {
        return(private$data$select(list(as.integer(indicies))))
      }
    },
    #----------------------------------------------------
    get_ids=function(){
      return(private$data["id"])
    },
    #----------------------------------------------------
    save=function(dir_path,create_dir=TRUE){
      if(dir.exists(dir_path)==FALSE){
        if(create_dir==TRUE){
          dir.create(dir_path)
        } else {
          stop("Directory does not exist.")
        }
      }
      private$data$save_to_disk(dataset_path=dir_path)
    },
    #----------------------------------------------------------------------
    load=function(dir_path){
      private$data=datasets$Dataset$load_from_disk(dataset_path = dir_path)
    }
  ),
  private = list(
    data=NULL,
    #--------------------------------------------------------------------------
    add=function(new_dataset){
      if(is.null(private$data)){
        private$data=new_dataset
      } else {
        private$data=datasets$concatenate_datasets(
          list(private$data,new_dataset))
      }
    }
  )
)




