stargazer_sfa <- function(models, 
                          model_names = NULL
                          ){
                  # models an SFA object in a vector or list
                  # model_names string vector for column names
  
  # 
  if(class(models) =="list") {
  obj <- stargazer_sfa_lst(models, column_labels= model_names)
  } 
  if(class(models) =="character") {
   obj <- stargazer_sfa_vec(models, column_labels= model_names)
  } 
  return( obj)
}