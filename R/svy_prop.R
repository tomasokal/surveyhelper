#' svy_prop
#' 
#' This is helper function to create a table of survey frequency proportions with one row variable and one column variable.
#'
#' @param row Row variable from survey object.
#' @param column Column variable from survey object.
#' @param data Survey object.
#'
#' @return A dataframe with survey frequencies put into a table of proportions.
#'
#' @import survey
#'
#' @examples svy_prop(row = "b1a", column = "raceth", data = svy_df)
svy_prop <- function(row, column, data) {
  
  formula <- as.formula(paste0("~", row, "+", column))
  df <- as.data.frame(svytable(formula, design = data, Ntotal = 100))
  df_prop <- as.data.frame(tapply(df[, 3], list(df[, 2], df[, 1]), mean))
  
  return(df_prop)
  
}
