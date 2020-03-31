#' svy_compare
#' 
#' This is a helper function to get counts of how many times each combination of levels of a provided vector of variables appears in the provided dataset. Useful for tracking programmatic logic.
#'
#' @param variables Vector of columns from survey object.
#' @param data Survey object.
#'
#' @return A data.frame with counts of each possible combination.
#' 
#' @import survey
#'
svy_compare <- function(variables, data) {
  
  # Use unique(expand.grid()) on the vector of variables.
  
  # Get counts of how many times each combination appears in the provided dataset.
  
}
