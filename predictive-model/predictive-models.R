###############################################################################
# Power analysis for Conjoint Experiments: People vs. Trials                  #
# Authors: Alberto Stefanelli & Martin Lukac                                  #
# Models used to interpolate between the simulation points                    #
# Used for the shiny app                                                      #          
###############################################################################

# 0. Libraries and Load in -------------------------------------------------
library(tidyverse)

# Collection of predictive models used

# Load the simulation
simulation_pwr <- readRDS("simulation_results.rds")
# remove first line since it is NA
simulation_pwr <- simulation_pwr[-1,]

# 1. Statistical Power Model --------------------------------------------------
modP <- glm(sig_robust ~ log(num_respondents) + 
               log(num_tasks) + 
               log(true_coef) + 
               log(num_lvls) +
               I(log(num_respondents) ^ 2) + 
               I(log(num_tasks) ^ 2) + 
               I(log(true_coef) ^ 2) + 
               I(log(num_lvls) ^ 2) +
               I(log(num_respondents) ^ 3) + 
               I(log(num_tasks) ^ 3) + 
               I(log(true_coef) ^ 3) + 
               I(log(num_lvls) ^ 3) +
               I(log(num_respondents) ^ 4) + 
               I(log(num_tasks) ^ 4) + 
               I(log(true_coef) ^ 4) + 
               I(log(num_lvls) ^ 4) +
               log(num_respondents) : log(num_tasks) +
               log(num_respondents) : log(true_coef) +
               log(num_respondents) : log(num_lvls) +
               log(num_tasks) : log(true_coef) +
               log(num_tasks) : log(num_lvls) +
               log(true_coef) : log(num_lvls) +
               log(num_respondents) : log(num_tasks) : log(true_coef) +
               log(num_respondents) : log(num_tasks) : log(num_lvls) +
               log(num_respondents) : log(true_coef) : log(num_lvls) +
               log(num_tasks) : log(true_coef) : log(num_lvls) +
               log(num_respondents) : log(num_tasks) : log(true_coef) : log(num_lvls),
             data = simulation_pwr,
             family = binomial(link = "logit"))

# 1.1 Export coefficients Type M Model for shiny app --------------------------
#readr::write_csv(data.frame(V1=array(coef(modP))),"glm_coefs_pwr.csv")

# 2. Type S Model -------------------------------------------------------------
simulation_typeS <- simulation_pwr %>%
  mutate(typeS_robust_rec = ifelse(typeS_robust == 1, 0,
                                    ifelse(typeS_robust == 0, 1,
                                           NA)))

modS <- glm(typeS_robust_rec ~ log(num_respondents) + 
              log(num_tasks) + 
              log(true_coef) + 
              log(num_lvls) +
              I(log(num_respondents) ^ 2) + 
              I(log(num_tasks) ^ 2) + 
              I(log(true_coef) ^ 2) + 
              I(log(num_lvls) ^ 2) +
              I(log(num_respondents) ^ 3) + 
              I(log(num_tasks) ^ 3) + 
              I(log(true_coef) ^ 3) + 
              I(log(num_lvls) ^ 3) +
              I(log(num_respondents) ^ 4) + 
              I(log(num_tasks) ^ 4) + 
              I(log(true_coef) ^ 4) + 
              I(log(num_lvls) ^ 4) +
              log(num_respondents) : log(num_tasks) +
              log(num_respondents) : log(true_coef) +
              log(num_respondents) : log(num_lvls) +
              log(num_tasks) : log(true_coef) +
              log(num_tasks) : log(num_lvls) +
              log(true_coef) : log(num_lvls) +
              log(num_respondents) : log(num_tasks) : log(true_coef) +
              log(num_respondents) : log(num_tasks) : log(num_lvls) +
              log(num_respondents) : log(true_coef) : log(num_lvls) +
              log(num_tasks) : log(true_coef) : log(num_lvls) +
              log(num_respondents) : log(num_tasks) : log(true_coef) : log(num_lvls),
            data = simulation_typeS,
            family = binomial(link = "logit"))

# 2.1 Export coefficients Type M Model for shiny app --------------------------
#readr::write_csv(data.frame(V1=array(coef(modS))),"glm_coefs_typeS.csv")


# 3. Type M Model -------------------------------------------------------------
simulation_typeM <- simulation_pwr %>%
  mutate(typeM_robust_rec = ifelse(typeM_robust < 0, NA, typeM_robust))


modM <- glm(log(typeM_robust_rec) ~ log(num_respondents) + 
                log(num_tasks) + 
                log(true_coef) + 
                log(num_lvls) +
                I(log(num_respondents) ^ 2) + 
                I(log(num_tasks) ^ 2) + 
                I(log(true_coef) ^ 2) + 
                I(log(num_lvls) ^ 2) +
                I(log(num_respondents) ^ 3) + 
                I(log(num_tasks) ^ 3) + 
                I(log(true_coef) ^ 3) + 
                I(log(num_lvls) ^ 3) +
                I(log(num_respondents) ^ 4) + 
                I(log(num_tasks) ^ 4) + 
                I(log(true_coef) ^ 4) + 
                I(log(num_lvls) ^ 4) +
                log(num_respondents) : log(num_tasks) +
                log(num_respondents) : log(true_coef) +
                log(num_respondents) : log(num_lvls) +
                log(num_tasks) : log(true_coef) +
                log(num_tasks) : log(num_lvls) +
                log(true_coef) : log(num_lvls) +
                log(num_respondents) : log(num_tasks) : log(true_coef) +
                log(num_respondents) : log(num_tasks) : log(num_lvls) +
                log(num_respondents) : log(true_coef) : log(num_lvls) +
                log(num_tasks) : log(true_coef) : log(num_lvls) +
                log(num_respondents) : log(num_tasks) : log(true_coef) : log(num_lvls),
              data = simulation_typeM,
              family = gaussian)

# 3.1 Export coefficients Type M Model for shiny app -----------------------------
#readr::write_csv(data.frame(V1=array(coef(modM))),"glm_coefs_typeM.csv")

# 4. Clean-up  -----------------------------===-----------------------------------
rm(simulation_pwr)
rm(simulation_typeM)
rm(simulation_typeS)