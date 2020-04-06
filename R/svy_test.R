# Necessary packages to load
library(survey)
library(multcomp)

# Data
data(api)

# Survey design setup
dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

# SVYGLM
glm_model <- survey::svyglm(ell ~ mobility, 
                            design = dclus1)

pw <- summary(multcomp::glht(glm_model))
pairwise.comps <- data.frame(names(pw$test$coefficients), pw$test$coefficients, pw$test$pvalues)


design.RDDphase <- IISclaf_df %>% as_survey_design(ids = CASEID,
                                                   nest = TRUE,
                                                   strata = STRATUM,
                                                   weights = RDDWT_D)

glmmodel <- svyglm(iis.participation.indicator ~ targetvar, design= design.RDDphase, subset=(PRC.response==1))

pw <- summary(glht(glmmodel, mcp(targetvar="Tukey")))  # Adjusted p-values and SEs

pairwise.comps <- data.frame(names(pw$test$coefficients),pw$test$coefficients,pw$test$pvalues)
colnames(pairwise.comps) <- c("Comparison", "Difference", "Pvalue")
