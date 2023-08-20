###############################################################################
# Power analysis for Conjoint Experiments: People vs. Trials                  #
# Authors: Alberto Stefanelli & Martin Lukac                                  #
# The models used to interpolate between the simulation points are applied to #
# each of the coefficients in the literature review.                          #
# Power, Type S and Type M are predicted and summarized in a figure           #    
###############################################################################


# 0. Libraries and Load in -------------------------------------------------
library("tidyverse")
library("latex2exp")
source("predictive-model/predictive-models.R")
library("here")

# Load the simulation
simulation <- readRDS(here("simulation_results.rds"))
# remove first line since it is NA (dk why)
simulation <- simulation[-1,]

# 1. Load in literature review and filter studies outside of sim range --------
# retro power for minimal discovered effect 

literature_review <-  read_delim(here("literature-review","lit_review_metadata.csv"), 
                                 ";", escape_double = FALSE, 
                                 locale = locale(encoding = "UTF-8"), 
                                 trim_ws = TRUE) %>%
  select(id, sample,n_levels_biggests_attribute, n_sample, tasks) %>%
  mutate(unique_id = paste0(id,sample)) %>% 
  distinct_at(vars(unique_id), .keep_all = TRUE)%>% 
  filter(n_levels_biggests_attribute < 21) %>%
  filter(n_sample < 3000) %>%
  filter(as.numeric(tasks) < 10) %>%
  select(-id, -sample, -unique_id)
  
tc_seq <- seq(0.01, 0.20, 0.01)
true_coef <- rep(tc_seq, each = nrow(literature_review))

litrev <- data.frame(
  num_respondents = rep(as.numeric(literature_review$n_sample), 
                        times = length(tc_seq)),
  num_tasks = rep(as.numeric(literature_review$tasks), 
                  times = length(tc_seq)),
  num_lvls = rep(as.numeric(literature_review$n_levels_biggests_attribute), 
                 times = length(tc_seq)),
  true_coef = true_coef)


# 2. Predict power, Type S, Type M --------------------------------------------
# imported from predictive-models.R

litrev <- litrev %>%
  mutate(
    power = predict(modP, litrev, type = "response"),
    typeS = predict(modS, litrev, type = "response"),
    typeM = exp(predict(modM, litrev, type = "response"))
  )


# 3. load in the new simul

# 3. load in the new simulation results  
# the new simulation is only for those articles that have been previously excluded (see line 22-24)
# this allows us to avoid extrapolation and get more reliable estimates.
# NB: the calculations for the studies that fell outside of the initial simulation are NOT based on a interpolating model but are straight from the simulation 

simulation_results_combined_lit_review <- readr::read_csv("predictive-model/simulation_results_combined_lit_review.csv")
simulation_results_combined_lit_review <- simulation_results_combined_lit_review[-1,]

summary_new_simulation <- simulation_results_combined_lit_review %>%
  mutate_at(vars(true_coef),round,2) %>%
  group_by(true_coef,num_respondents,num_tasks,num_lvls) %>%
  summarise(
    power = mean(sig_robust),
    typeS = 1-mean(typeS_robust,na.rm=T),
    typeM = mean(typeM_robust,na.rm=T))

# stack the interpolated results and the new simulation results 
litrev <- rbind(litrev, summary_new_simulation)
# Summaries into means
lreval <- litrev %>%
  # need to round otherwise dplyr messes up due to floating point 
  mutate_at(vars(true_coef),round,2) %>%
  group_by(true_coef) %>%
  summarise(
    mean_power = mean(power),
    mean_typeS = mean(typeS),
    mean_typeM = mean(typeM)
  )


# 4. Plot ---------------------------------------------------------------------
# 4A. Power ----
lr_pwr <- lreval %>%
  ggplot(aes(x = true_coef, y = mean_power)) +
  geom_hline(yintercept = 0.8, linetype = "dashed",
             color = "grey20") +
  geom_hline(yintercept = 0,
             color = "grey20") +
  geom_point(aes(x = true_coef, y = power,
                 color = power),
             data = litrev,
             alpha = 0.9) +
  geom_rect(data = data.frame(
    xminimum = 0.020, xmaximum = 0.12,
    yminimum = -1, ymaximum = 1.1
  ),
  inherit.aes = FALSE,
  mapping = aes(
    xmin = xminimum, xmax = xmaximum,
    ymin = yminimum, ymax = ymaximum
  ), color = "transparent", 
  fill = "grey20", alpha = 0.2) +
  scale_color_gradient2(
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
    limits = c(0, 1),
    midpoint = 0.8,
    high = "#0D0887FF",   # or  scales::muted("darkblue")
    low = "#E16462FF"     # and scales::muted("red")
  ) +
  geom_line() +
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(0, 0.15)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.position = "none"
  ) +
  ylab("Predicted power") + 
  xlab(latex2exp::TeX("Assumed AMCE ($\\delta$)")) +
  scale_x_continuous(breaks = c(seq(0, 0.15, 0.01)),
                     labels = c("0", "", "",
                                ".03", "", "",
                                ".06", "", "",
                                ".09", "", "",
                                ".12", "", "",
                                ".15")) +
  scale_y_continuous(breaks = c(seq(0, 1, 0.2)),
                     labels = c("0", ".2", ".4", 
                                ".6", ".8", "1"))

# 4B. Type S ----
lr_typeS <- lreval %>%
  ggplot(aes(x = true_coef, y = mean_typeS)) + 
  geom_point(aes(x = true_coef, y = typeS,
                 color = typeS),
             data = litrev,
             alpha = 0.9) +
  geom_rect(data = data.frame(
    xminimum = 0.020, xmaximum = 0.12,
    yminimum = -1, ymaximum = 1.1
  ),
  inherit.aes = FALSE,
  mapping = aes(
    xmin = xminimum, xmax = xmaximum,
    ymin = yminimum, ymax = ymaximum
  ), color = "transparent", 
  fill = "grey20", alpha = 0.2) +
  scale_color_gradient2(
    breaks = NULL,
    low = "black",
    high = "#E16462FF",
    midpoint = 0.05
  ) +
  geom_hline(yintercept = 0,
             color = "grey20") +
  geom_line() +
  theme_bw() +
  coord_cartesian(ylim = c(0, 0.3),
                  xlim = c(0, 0.16)) +
  scale_x_continuous(breaks = c(seq(0, 0.15, 0.01)),
                     labels = c("0", "", "",
                                ".03", "", "",
                                ".06", "", "",
                                ".09", "", "",
                                ".12", "", "",
                                ".15")) +
  ylab("Predicted Type S") + 
  xlab(latex2exp::TeX("Assumed AMCE ($\\delta$)")) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  ) +
  scale_y_continuous(breaks = c(seq(0, 0.3, 0.05)),
                     labels = c("0", ".05", ".10",
                                ".15", ".20", ".25",
                                ".30")) 

# 4C. Type M ----

lr_typeM <- lreval %>%
  ggplot(aes(x = true_coef, y = mean_typeM)) + 
  geom_point(aes(x = true_coef, y = typeM,
                 color = typeM),
             data = litrev,
             alpha = 0.95) +
  geom_rect(data = data.frame(
    xminimum = 0.020, xmaximum = 0.12,
    yminimum = -5, ymaximum = 15
  ),
  inherit.aes = FALSE,
  mapping = aes(
    xmin = xminimum, xmax = xmaximum,
    ymin = yminimum, ymax = ymaximum
  ), color = "transparent", 
  fill = "grey20", alpha = 0.2) +
  scale_color_gradient2(
    breaks = NULL,
    low = "black",
    high = "#E16462FF",
    midpoint = 2.5
  ) +
  geom_line() +
  geom_hline(yintercept = 0,
             color = "grey20") +
  geom_hline(yintercept = 1,
             color = "grey20",
             linetype = "dashed") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  ) +
  scale_x_continuous(breaks = c(seq(0, 0.15, 0.01)),
                     labels = c("0", "", "",
                                ".03", "", "",
                                ".06", "", "",
                                ".09", "", "",
                                ".12", "", "",
                                ".15"))  +
  coord_cartesian(ylim = c(0, 13),
                  xlim = c(0, 0.16)) +
  ylab("Predicted Type M") + 
  xlab(latex2exp::TeX("Assumed AMCE ($\\delta$)"))


ggpubr::ggarrange(lr_pwr + xlab(""), 
                  lr_typeS, 
                  lr_typeM + xlab(""),
                  nrow = 1, ncol = 3)

ggsave("figures/F7_litrev_power.png", 
       width = 7.5, height = 2.3,
       dpi = 900)

##### calculating retro power for various intervals of effect size #####

# lower than .03 (included)
true_0 <- litrev %>% filter(true_coef<=0.03)
paste(round(sum(true_0$power <= 0.80)/length(true_0$power),2)*100, "% of the studies under review are underpowered <.8")
true_0 %>% summarize(mean(typeS))
true_0 %>% summarize(mean(typeM))

# between .03 (excluded) and .06 (included) 
true_03 <- litrev %>% filter(true_coef<=0.06 & true_coef>0.03)
paste(round(sum(true_03$power <= .80)/length(true_03$power),2)*100, "% of the studies under review are underpowered <.8")
true_03 %>% summarize(mean(typeS))
true_03 %>% summarize(mean(typeM))

# between .06 (excluded) and .09 (included) 
true_06 <- litrev %>% filter(true_coef>0.06 & true_coef<=0.09)
paste(round(sum(true_06$power <= .80)/length(true_06$power),2)*100, "% of the studies under review are underpowered <.8")
true_06 %>% summarize(mean(typeS))
true_06 %>% summarize(mean(typeM))

# greater than 0.9 (excluded)
true_09 <- litrev %>% filter(true_coef>0.09)
paste(round(sum(true_09$power <= .80)/length(true_09$power),2)*100, "% of the studies under review are underpowered <.8")
true_09 %>% summarize(mean(typeS))
true_09 %>% summarize(mean(typeM))


##### calculating quantiles for literature review #####

literature_review_summary <-  read_delim(here("literature-review","lit_review_metadata.csv"), 
                                         ";", escape_double = FALSE, 
                                         locale = locale(encoding = "UTF-8"), 
                                         trim_ws = TRUE) %>%
  select(n_levels_biggests_attribute, n_sample,tot_attributes, tasks,coef, significance) %>%
  mutate(coef = abs(coef)) %>% 
  mutate(coef = as.numeric(coef)) %>%
  mutate(effective = n_sample*tasks) %>% 
  
summary(literature_review_summary)

# calculating % AMCEs that are significant 
literature_review_summary_sig <-  read_delim(here("literature-review","lit_review_metadata.csv"), 
                                         ";", escape_double = FALSE, 
                                         locale = locale(encoding = "UTF-8"), 
                                         trim_ws = TRUE) %>%
  select(n_levels_biggests_attribute, n_sample,tot_attributes, tasks,coef, significance) %>%
  mutate(coef = abs(coef)) %>% 
  mutate(coef = as.numeric(coef)) %>%
  mutate(effective = n_sample*tasks) %>% 
  filter(significance==1)


###### Predict number of tasks keeping power fixed #####

litrev <-  expand.grid(num_respondents =seq(500, 3000, 10),
                       num_tasks = seq(1, 10, 1),
                       num_lvls = seq(1,10,1),
                       true_coef = seq(0.01, 0.20, 0.01))


litrev <- litrev %>%
  mutate(
    power = predict(modP, litrev, type = "response")
  )


modT <- glm(log(num_tasks) ~ log(num_respondents) + 
              log(power) + 
              log(true_coef) + 
              log(num_lvls) +
              I(log(num_respondents) ^ 2) + 
              I(log(power) ^ 2) + 
              I(log(num_lvls) ^ 2) +
              I(log(num_respondents) ^ 3) + 
              I(log(power) ^ 3) + 
              I(log(num_lvls) ^ 3) +
              I(log(num_respondents) ^ 4) + 
              I(log(power) ^ 4) + 
              I(log(num_lvls) ^ 4) +
              log(num_respondents) : log(power) +
              log(num_respondents) : log(true_coef) +
              log(num_respondents) : log(num_lvls) +
              log(power) : log(true_coef) +
              log(power) : log(num_lvls) +
              log(true_coef) : log(num_lvls) +
              log(num_respondents) : log(power) : log(true_coef) +
              log(num_respondents) : log(power) : log(num_lvls) +
              log(num_respondents) : log(true_coef) : log(num_lvls) +
              log(power) : log(true_coef) : log(num_lvls) +
              log(num_respondents) : log(power) : log(true_coef) : log(num_lvls),
            data = litrevp,
            family = gaussian)

summary(modT)

litrev_s <- data.frame(
  num_respondents = rep(seq(500, 6000, 100), 
                        times = length(effect)),
  power = rep(0.8, 
              times = length(effect)),
  num_lvls = rep(2, 
                 times = length(effect)),
  true_coef = c(0.1))


litrev_s <- litrev_s %>%
  mutate(
    task = exp(predict(modT, litrev_s, type = "response"))
  )

