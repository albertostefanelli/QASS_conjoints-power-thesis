###############################################################################
# Power analysis for Conjoint Experiments: People vs. Trials                  #
# Authors: Alberto Stefanelli & Martin Lukac                                  #
# Evaluation for the models used to interpolate between the simulation points #
# This is used also to validate the results of the shiny app.                 #          
###############################################################################

# 0. Libraries and Load in -------------------------------------------------
library(tidyverse)

# Load the simulation
simulation <- readRDS("simulation_results.rds")
# remove first line since it is NA (dk why)
simulation <- simulation[-1,]

# 1. Evaluation of the model of Power -----------------------------------------
# Null model
mod1 <- glm(sig_robust ~ num_respondents + num_tasks + true_coef + num_lvls +
              I(num_respondents ^ 2) + 
              I(num_tasks ^ 2) +
              I(true_coef ^ 2) +
              I(num_lvls ^ 2) + 
              I(num_respondents ^ 3) + 
              I(num_tasks ^ 3) +
              I(true_coef ^ 3) +
              I(num_lvls ^ 3) +
              num_respondents : num_tasks +
              num_respondents : true_coef +
              num_respondents : num_lvls +
              num_tasks : true_coef +
              num_tasks : num_lvls +
              true_coef : num_lvls +
              num_respondents : num_tasks : true_coef +
              num_respondents : num_tasks : num_lvls +
              num_respondents : true_coef : num_lvls +
              num_tasks : true_coef : num_lvls +
              num_respondents : num_tasks : true_coef : num_lvls,
            data = simulation,
            family = binomial(link = "logit"))

# Extended model

modl4 <- glm(sig_robust ~ log(num_respondents) + 
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
             data = simulation,
             family = binomial(link = "logit"))

## Evaluate power simulation
simulation %>%
  group_by(num_respondents,
           num_tasks,
           true_coef,
           num_lvls) %>%
  summarize(power_robust = mean(sig_robust)) -> ut_grid_pwr

ut_grid_pwr$pred_mod1 <- predict(mod1, newdata = ut_grid_pwr, type = "response")
ut_grid_pwr$pred_modl4 <- predict(modl4, newdata = ut_grid_pwr, type = "response")

ut_grid_pwr$diff_mod1 <- ut_grid_pwr$power_robust - ut_grid_pwr$pred_mod1
ut_grid_pwr$diff_modl4 <- ut_grid_pwr$power_robust - ut_grid_pwr$pred_modl4

rm(mod1)

# Plot results for modl4
ut_grid_pwr %>%
  ggplot(aes(x = num_respondents, y = diff_modl4)) +
  geom_hline(yintercept = 0.05, color = "red", size = 1.05) +
  geom_hline(yintercept = -0.05, color = "red", size = 1.05) +
  geom_point() +
  facet_grid(num_tasks ~ true_coef) +
  geom_hline(yintercept = 0, color = "blue") +
  theme_bw() +
  #coord_cartesian(ylim = c(-.1, .1)) +
  ylab("Error") + xlab("Number of respondents") +
  theme(
    panel.grid = element_blank()
  )

rmse_reg(modl4)

ut_grid_pwr %>%
  ungroup() %>%
  summarize(RMSE_mod1 = sqrt(mean(diff_mod1 ^ 2)),
            RMSE_modl4 = sqrt(mean(diff_modl4 ^ 2)))


# fairly similar RMSE, but I go with modl4
# Model 4 on average the prediction error is ~2.3%

# 2. Evaluation of the model of Type S ----------------------------------------
simulation_typeS <- simulation
simulation_typeS$typeS_robust_rec <- ifelse(simulation$typeS_robust == 1, 0,
                                      ifelse(simulation$typeS_robust == 0, 1,
                                             NA))

# Estimate a logistic regression model of Type S error occurence
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
summary(modS)

# install.packages("equatiomatic")
# equatiomatic::extract_eq(modS)

simulation_typeS %>%
  group_by(num_respondents,
           num_tasks,
           true_coef,
           num_lvls) %>%
  summarize(typeS_prob = mean(typeS_robust_rec, na.rm = T)) -> ut_grid_S

# Calculate the predicted Type S error and the difference from the simulation
ut_grid_S$pred_modS_1 <- predict(modS, newdata = ut_grid_S, type = "response")
ut_grid_S$diff_modS_1 <- ut_grid_S$typeS_prob - ut_grid_S$pred_modS_1

ut_grid_S %>%
  ungroup() %>%
  summarize(RMSE_modS_1 = sqrt(mean(diff_modS_1 ^ 2)))
# on average the prediction error is ~1.3%

# Plot the differences
ut_grid_S %>%
  ggplot(aes(x = num_respondents, y = diff_modS_1)) +
  #geom_hline(yintercept = 0.25, color = "red", size = 1.15) +
  #geom_hline(yintercept = -0.25, color = "red", size = 1.15) +
  geom_point() +
  facet_grid(num_tasks ~ true_coef) +
  geom_hline(yintercept = 0, color = "blue") +
  theme_bw()


# 3. Evaluation of the model of Type M ----------------------------------------

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
simulation_typeM <- simulation %>%
 mutate(typeM_robust_rec = ifelse(typeM_robust < 0, NA, typeM_robust))
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# trying to see if we can predict better estimating polynomial interactions 
# for now i see only very small gains in RMSA

# modM_1 <- glm(log(typeM_robust_rec) ~ log(num_respondents) + 
#                 log(num_tasks) + 
#                 log(true_coef) + 
#                 log(num_lvls) +
#                 I(log(num_respondents) ^ 2) + 
#                 I(log(num_tasks) ^ 2) + 
#                 I(log(true_coef) ^ 2) + 
#                 I(log(num_lvls) ^ 2) +
#                 I(log(num_respondents) ^ 3) + 
#                 I(log(num_tasks) ^ 3) + 
#                 I(log(true_coef) ^ 3) + 
#                 I(log(num_lvls) ^ 3) +
#                 I(log(num_respondents) ^ 4) + 
#                 I(log(num_tasks) ^ 4) + 
#                 I(log(true_coef) ^ 4) + 
#                 I(log(num_lvls) ^ 4) +
#                 I(log(num_respondents) ^ 5) + 
#                 I(log(num_tasks) ^ 5) + 
#                 I(log(true_coef) ^ 5) + 
#                 I(log(num_lvls) ^ 5) +
#                 log(num_respondents) : log(num_tasks) +
#                 log(num_respondents) : log(true_coef) +
#                 log(num_respondents) : log(num_lvls) +
#                 I(log(num_respondents) ^ 2) : log(num_tasks) +
#                 I(log(num_respondents) ^ 2): log(true_coef) +
#                 I(log(num_respondents) ^ 2) : log(num_lvls) +
#                 log(num_tasks) : log(true_coef) +
#                 log(num_tasks) : log(num_lvls) +
#                 log(true_coef) : log(num_lvls) +
#                 I(log(num_tasks) ^ 2) : log(true_coef) +
#                 I(log(num_tasks) ^ 2) : log(num_lvls) +
#                 I(log(num_tasks) ^ 2) : log(num_lvls) +
#                 
#                 log(num_respondents) : log(num_tasks) : log(true_coef) +
#                 log(num_respondents) : log(num_tasks) : log(num_lvls) +
#                 log(num_respondents) : log(true_coef) : log(num_lvls) +
#                 I(log(num_respondents) ^ 2)  : log(num_tasks) : log(true_coef) +
#                 I(log(num_respondents) ^ 2)  : log(num_tasks) : log(num_lvls) +
#                 I(log(num_respondents) ^ 2)  : log(true_coef) : log(num_lvls) +
#                 
#                 log(num_tasks) : log(true_coef) : log(num_lvls) +
#                 
#                 log(num_respondents) : log(num_tasks) : log(true_coef) : log(num_lvls) +
#                 I(log(num_respondents)^ 2) : I(log(num_tasks)^ 2) : I(log(true_coef)^ 2) : I(log(num_lvls)^ 2),
#               
#               data = simulation_typeM,
#               family = gaussian)

# added poly degree 5 to push RMSA below 9 
modM_1 <- glm(log(typeM_robust_rec) ~ log(num_respondents) + 
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
               I(log(num_respondents) ^ 5) + 
               I(log(num_tasks) ^ 5) + 
               I(log(true_coef) ^ 5) + 
               I(log(num_lvls) ^ 5) +
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

summary(modM_1)

ut_grid_M <-  simulation_typeM %>%
  group_by(num_respondents,
           num_tasks,
           true_coef,
           num_lvls) %>%
  mutate(typeMr_log = log(typeM_robust_rec)) %>% 
  summarize(typeMr_log_mean = mean(typeMr_log,na.rm=T), .groups = 'keep')

# Calculate the predicted Type M error and the difference from the simulation
ut_grid_M$pred_modM_1 <- predict(modM_1, newdata = ut_grid_M, type = "response")
ut_grid_M$diff_modM_1 <- ut_grid_M$typeMr_log_mean - ut_grid_M$pred_modM_1

ut_grid_M %>%
  ungroup() %>%
  summarize(RMSE_mod1 = sqrt(mean(diff_modM_1 ^ 2)))
# on average the prediction error is ~9%

# polynomial of degree 9 does not fit 
# polynomial of degree 10 does not fit 

# Plot the differences
ut_grid_M %>%
  ggplot(aes(x = num_respondents, y = diff_modM_1)) +
  geom_hline(yintercept = 0.25, color = "red", size = 1.15) +
  geom_hline(yintercept = -0.25, color = "red", size = 1.15) +
  geom_point() +
  facet_grid(num_tasks ~ true_coef) +
  geom_hline(yintercept = 0, color = "blue") +
  theme_bw()

### --------------------- ###
### Plotting for PAPER ###
### --------------------- ###

error_pwr <- ut_grid_pwr %>%
  ggplot(aes(x = num_respondents, y = diff_modl4)) +
  geom_hline(yintercept = 0.05, color = "red", size = 0.5,linetype="dashed") +
  geom_hline(yintercept = -0.05, color = "red", size = 0.5,linetype="dashed") +
  geom_point(alpha = 0.4) +
  facet_grid(~ true_coef) +
  geom_hline(yintercept = 0, color = "blue") +
  theme_bw() +
  #coord_cartesian(ylim = c(-.3, .3)) +
  ylab("Error") + xlab("Number of respondents") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 7)
  )

error_s <- ut_grid_S %>%
  ggplot(aes(x = num_respondents, y = diff_modS_1)) +
  geom_hline(yintercept = 0.05, color = "red", size = 0.5,linetype="dashed") +
  geom_hline(yintercept = -0.05, color = "red", size = 0.5,linetype="dashed") +
  geom_point(alpha = 0.4) +
  facet_grid( ~ true_coef) +
  geom_hline(yintercept = 0, color = "blue") +
  theme_bw() +
  #coord_cartesian(ylim = c(-.3, .3)) +
  ylab("Error") + xlab("Number of respondents") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 7)
  )

error_m <- ut_grid_M %>%
  ggplot(aes(x = num_respondents, y = diff_modM_1)) +
  geom_hline(yintercept = 0.1, color = "red", size = 0.5,linetype="dashed") +
  geom_hline(yintercept = -0.1, color = "red", size = 0.5,linetype="dashed") +
  geom_point(alpha = 0.4) +
  facet_grid(~ true_coef) +
  geom_hline(yintercept = 0, color = "blue") +
  theme_bw() +
  #coord_cartesian(ylim = c(-.3, .3)) +
  ylab("Error") + xlab("Number of respondents") +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 7)
  )

ggpubr::ggarrange(error_pwr+ ylab("Prediction error \n power") + xlab(""), 
                  error_s + ylab("Prediction error \n Type S error") + xlab(""),
                  error_m + ylab("Prediction error \n Type M error"),
                  ncol = 1, nrow = 3,
                  common.legend = T)


ggsave("figures/A1_error_rate_model.png", height = 4, width = 10,
       dpi = 900)


