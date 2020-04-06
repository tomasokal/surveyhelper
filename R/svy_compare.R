#' svy_compare
#' 
#' This is a helper function to get counts of how many times each combination of levels of a provided vector of variables appears in the provided dataset. Useful for tracking programmatic logic.
#'
#' @param variables Vector of columns from data.
#' @param data A data object that can be either a survey object or other data type.
#'
#' @return A data.frame with counts of each possible combination.
#' 
#' @export
#' 
#' @import survey
#' @import data.table
#' @importFrom methods is
#' 
svy_compare <- function(variables, data) {
  
  if (methods::is(data, "survey.design") == TRUE | methods::is(data, "survey.design2") == TRUE) {
  
    dt <- data.table::setDT(data[["variables"]])
    
    dt_output <- dt[
      
      ,
      .N
      ,
      by = mget(variables)
      ]
  
  }
  
  else {
    
    dt <- data.table::setDT(data)
    
    dt_output <- dt[
      
      ,
      .N
      ,
      by = mget(variables)
      ]
    
  }  
  
  dt_output <- dt_output[do.call(order, dt_output), ]
  
  return(dt_output)
  
}
