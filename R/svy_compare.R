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
#' @import data.table
#'
svy_compare <- function(variables, data) {
  
  # Use unique(expand.grid()) on the vector of variables.
  
  # Get counts of how many times each combination appears in the provided dataset.
  
}


library(data.table)
library(survey)


temp_svy$variables$pid1

unique(expand.grid(temp_svy$variables$pid1, temp_svy$variables$pida))

caseid <- seq(1:1000)
weight <- rnorm(1000, 1, 0.15)
var1 <- as.factor(sample(c("Yes", "No"), 1000, TRUE))

sample_df <- as.data.frame(cbind(caseid, weight, var1))

sample_df$var2 <- ifelse(sample_df$var1 == 1, sample(c("Yes", "No"), replace = TRUE), NA)

svy_df <- svydesign(ids = ~ caseid,
                    weights = ~ weight,
                    data = sample_df)

test <- svy_df[["variables"]]
dt <- setDT(test)
dt[
  , 
  .N
  , 
  by = .(var1, var2)
  ]

