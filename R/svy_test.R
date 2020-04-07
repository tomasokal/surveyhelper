# Current scratch work

## Necessary packages
library(survey)
library(multcomp)
library(data.table)

## Data used
df <- fread("P:\\AP-NORC Center\\Common\\AP Special Projects\\Omnibus\\2020\\03.2020\\AP March\\Data\\March_completes_clean_AP.csv")

## Process
dt <- df[, c("CaseId", "WEIGHT_ENES", "CUR1", "pid")]
  
  # Picking out caseid, weights, response variable, and group variable.

dt1 <- as.data.table(stats::model.matrix(~ CUR1 - 1, data = df))
dt2 <- as.data.table(stats::model.matrix(~ pid - 1, data = df))

  # Use model.matrix to create sets of dummy variables.

    # Issue exists that this adds in the variables as they appear in dataset.

dt_model <- as.data.table(cbind(dt[, -c(3, 4)], dt1, dt2))

  # cbind these outputs to the orginal dataframe where we also drop the original variables.

dclus1 <- survey::svydesign(id = ~ CaseId, weights = ~ WEIGHT_ENES, data = dt_model)

  # Generate survey design object.

cols <- grep("pid", names(dt_model))

  # Pick out columns that have the group variable in them.

group_var <- as.list(names(dt_model[, ..cols]))

  # Assign these into a list.
    
    # Will want more efficient way to do this.

## Funcitonal process

list_m <- lapply(group_var, function(x) svyglm(as.formula(paste0("`CUR1Right direction` ~ ", x)), design = dclus1))

  # Run svyglm over list of group variables. 

list_c <- lapply(list_m, function(x) summary(multcomp::glht(x)))

  # Put these models through multiple comparison function.

list_p <- lapply(list_c, function(x) as.data.frame(x[[9]][6]))

  # Extract p-values from these.


## Scrath work below

df$cur1_rightdirection <- ifelse(df$CUR1 %in% "Right direction", 1, 0)
df$cur1_wrongdirection <- ifelse(df$CUR1 %in% "Wrong direction", 1, 0)
df$pid_dem <- ifelse(df$pid %in% "Democrat", 1, 0)
df$pid_rep <- ifelse(df$pid %in% "Republican", 1, 0)
df$pid_ind <- ifelse(df$pid %in% "Independent", 1, 0)

dclus1 <- survey::svydesign(id = ~ CaseId,
                            weights = ~ WEIGHT_ENES,
                            data = dt_model)

glm_model <- survey::svyglm(cur1_rightdirection ~ pid_dem, 
                            design = dclus1)

pw <- summary(multcomp::glht(glm_model))
pairwise.comps <- data.frame(names(pw$test$coefficients), pw$test$coefficients, pw$test$pvalues)

### Ben scratch work

design.RDDphase <- IISclaf_df %>% as_survey_design(ids = CASEID,
                                                   nest = TRUE,
                                                   strata = STRATUM,
                                                   weights = RDDWT_D)

glmmodel <- svyglm(iis.participation.indicator ~ targetvar, design= design.RDDphase, subset=(PRC.response==1))

pw <- summary(glht(glmmodel, mcp(targetvar="Tukey")))  # Adjusted p-values and SEs

pairwise.comps <- data.frame(names(pw$test$coefficients),pw$test$coefficients,pw$test$pvalues)
colnames(pairwise.comps) <- c("Comparison", "Difference", "Pvalue")
