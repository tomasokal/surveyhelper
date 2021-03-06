#' svy_prop
#' 
#' This is helper function to create a table of survey frequency proportions with one row variable and one column variable.
#'
#' @param row Row variable from survey object.
#' @param column Column variable from survey object.
#' @param data Survey object.
#' @param orientation Specify whether proportions should be given rowwise or colwise.
#'
#' @return A dataframe with survey frequencies put into a table of proportions.
#' 
#' @export
#'
#' @import survey
#' @importFrom stats as.formula
#'
#' 
svy_prop <- function(row, column, data, orientation = NULL) {
  
  formula <- stats::as.formula(paste0("~", column, "+", row))
  df <- as.data.frame(survey::svytable(formula, design = data, Ntotal = 100))
  df_prop <- as.data.frame(tapply(df[, 3], list(df[, 2], df[, 1]), mean))
  
  if (missing(orientation)) {
    
    df_prop <- df_prop
    
  }
  
  else {
    
    if (orientation == "colwise") {
      
      df_prop <- apply(df_prop, 2, function(x) {x / sum(x)})
      df_prop[is.na(df_prop)] <- 0
      df_prop <- df_prop * 100
      
    }
    
    else if (orientation == "rowwise") {
      
      df_prop_sum <- apply(df_prop[, 1:ncol(df_prop)], 1, sum)
      df_prop <- df_prop / df_prop_sum
      df_prop[is.na(df_prop)] <- 0
      df_prop <- df_prop * 100
      
    }
    
    else {
      
      df_prop <- df_prop
      
      print("Please select either rowwise or colwise for orientation.")
      
    }
    
  }
  
  return(df_prop)
  
}
