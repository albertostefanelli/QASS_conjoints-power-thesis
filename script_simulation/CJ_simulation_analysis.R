###############################################################################
# Power analysis for Conjoint Experiments: People vs. Trials                  #
# Authors: Alberto Stefanelli & Martin Lukac                                  #
# Analysis of the simulation results                                          #
###############################################################################
 
#### 0. Libraries and Load in -------------------------------------------------
library(tidyverse)
library(latex2exp)

# Load the simulation
simulation <- readRDS("simulation_results.rds")

# remove first line since it is NA
simulation <- simulation[-1,]
head(simulation)

simulation$effective_sample <- simulation$num_respondents*simulation$num_tasks

#### 1.Sample considerations --------------------------------------------------

#### 1.a Figure 2 Statistical power given sample size - subjects & trials -----

## Subjects
sample_subj <- simulation %>%
  filter(num_tasks == 5) %>%
  filter(num_lvls == 5) %>%
  filter(true_coef %in% c(0.01, 0.03, 0.05, 0.1)) %>%
  group_by(true_coef, num_respondents) %>%
  summarize(power = mean(sig_robust))

plot_subj <- sample_subj %>%
  ggplot(aes(x = num_respondents, y = power, 
                             group = factor(true_coef), 
                             color = factor(true_coef))) +
  geom_line() +
  scale_color_viridis_d() +
  theme_bw() +
  geom_hline(yintercept = 0.8, linetype = 2) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 9),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 6)
  ) +
  scale_x_continuous(breaks = c(0, 1000, 2000, 3000),
                     labels = c("0", "1k", "2k", "3k")) +
  xlab("Respondents") + ylab("Power")

## Trials
sample_trials <- simulation %>%
  filter(num_respondents == 1750) %>%
  filter(num_lvls == 5) %>%
  filter(true_coef %in% c(0.01, 0.03, 0.05, 0.1)) %>%
  group_by(true_coef, num_tasks) %>%
  summarize(power = mean(sig_robust))

plot_trials <- sample_trials %>%
  ggplot(aes(x = num_tasks, y = power, 
             group = factor(true_coef), 
             color = factor(true_coef))) +
  geom_line() +
  scale_color_viridis_d() +
  theme_bw() +
  geom_hline(yintercept = 0.8, linetype = 2) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 9),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 6)
  ) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9)) +
  xlab("Trials") + ylab("Power")


## Effective N
sample_effn <- simulation %>%
  filter(num_lvls == 5) %>%
  filter(true_coef %in% c(0.01, 0.03, 0.05, 0.1)) %>%
  group_by(true_coef, effective_sample) %>%
  summarize(power = mean(sig_robust))

plot_effn <- sample_effn %>%
  ggplot(aes(x = effective_sample, y = power, 
             group = factor(true_coef), 
             color = factor(true_coef))) +
  geom_line() +
  scale_color_viridis_d() +
  theme_bw() +
  geom_hline(yintercept = 0.8, linetype = 2) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 9),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000),
                     labels = c("0", "5k", "10k", "15k", "20k", "25k", "30k")) +
  xlab("Effective sample") + ylab("Power")


ggpubr::ggarrange(plot_subj, 
                  plot_trials + ylab(""),
                  plot_effn + ylab(""),
                  ncol = 3, nrow = 1,
                  common.legend = T)
ggsave("figures/F2_power_vs_sample.png", 
       width = 5.5, height = 2,
       dpi = 900)


#### 1.b Figure 3 Statistical power given sample size - heatplot --------------

# Estimate a logistic regression model of power
mod1 <- glm(sig_robust ~ log(num_respondents) + 
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
summary(mod1)

# Export the model for Shiny
# rio::export(matrix(summary.glm(mod1)$coefficients[,1]), "glm_coefs.csv")

# Create a plotting function for the heatplot
plot_power_sample <- function(z){
  # Input:
  #   - z: expected effect size
  # Output:
  #   - ggplot object
  new <- expand.grid(num_respondents = seq(500, 3000, 10),
                     num_tasks = seq(1, 9, 0.1),
                     true_coef = z, #seq(0.01, 0.10, 0.01),
                     num_lvls = 2) #seq(2, 10, 1))
  new$pred_sig <- predict.glm(mod1, newdata = new, type = "response")
  new$effective_sample <- new$num_respondents * new$num_tasks
  
  
  ggplot(new, aes(num_respondents, num_tasks, fill = pred_sig)) +
    geom_raster(interpolate = F) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = c(1000, 2000, 3000),
                       labels = c("1k", "2k", "3k")) +
    scale_y_continuous(breaks = c(1, 3, 5, 7, 9)) +
    scale_fill_gradient2(
      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
      limits = c(0, 1),
      midpoint = 0.8,
      high = "#0D0887FF",   # or  scales::muted("darkblue")
      low = "#E16462FF"     # and scales::muted("red")
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 6),
      axis.title = element_text(size = 8),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 6)
    ) +
    xlab("Respondents") + ylab("Tasks") +
    guides(fill = guide_colourbar(barwidth = 20,
                                  barheight = 0.5,
                                  frame.colour = "black",
                                  frame.linewidth = 1,
                                  ticks.colour = "black",
                                  ticks.linewidth = 1)) +
    annotate("text", x = 2500, y = 8.5, size = 2.5,
             label = paste("delta ==", z), parse = T)
}

# Plot with different true_coefs
ggpubr::ggarrange(plot_power_sample(0.01) + xlab(""),
                  plot_power_sample(0.02) + xlab("") + ylab(""),
                  plot_power_sample(0.03) + ylab(""),
                  plot_power_sample(0.04) + xlab("") + ylab(""),
                  plot_power_sample(0.05) + xlab("") + ylab(""), 
                  ncol = 5, nrow = 1,
                  common.legend = T,
                  labels = c("Power", "", "", "", ""),
                  hjust = -3,
                  vjust = -2.5,
                  font.label = list(
                    face = "plain",
                    size = 9
                  ))
ggsave("figures/F3_respondents_tasks_power_2lvls.png", height = 1.75, width = 7,
       dpi = 900)

#### 2. Experiment design considerations --------------------------------------

#### 2.a Figure 4 Attributes vs levels ----------------------------------------
plot_power_levels <- function(z){
  # Input:
  #   - z: expected number of levels of a variable
  # Output:
  #   - ggplot object
  new <- expand.grid(num_respondents = seq(500, 3000, 10),
                     num_tasks = seq(1, 9, 0.1),
                     true_coef = 0.05,
                     num_lvls = z)
  new$pred_sig <- predict.glm(mod1, newdata = new, type = "response")
  new$effective_sample <- new$num_respondents * new$num_tasks
  
  
  ggplot(new, aes(num_respondents, num_tasks, fill = pred_sig)) +
    geom_raster(interpolate = F) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = c(1000, 2000, 3000, 4000, 5000),
                       labels = c("1k", "2k", "3k", "4k", "5k")) +
    scale_y_continuous(breaks = c(1, 3, 5, 7, 9)) +
    scale_fill_gradient2(
      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
      limits = c(0, 1),
      midpoint = 0.8,
      high = "#0D0887FF",   # or  scales::muted("darkblue")
      low = "#E16462FF"     # and scales::muted("red")
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 6),
      axis.title = element_text(size = 8),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 6)
    ) +
    xlab("Respondents") + ylab("Tasks") +
    guides(fill = guide_colourbar(barwidth = 20,
                                  barheight = 0.5,
                                  frame.colour = "black",
                                  frame.linewidth = 1,
                                  ticks.colour = "black",
                                  ticks.linewidth = 1)) +
    annotate("text", x = 2350, y = 8.25, size = 2.5,
             label = paste(z, "levels"), parse = F)
}

# Plot with different true_coefs
ggpubr::ggarrange(plot_power_levels(2) + xlab(""),
                  plot_power_levels(4) + xlab("") + ylab(""),
                  plot_power_levels(6) + ylab(""),
                  plot_power_levels(8) + xlab("") + ylab(""),
                  plot_power_levels(10) + xlab("") + ylab(""), 
                  ncol = 5, nrow = 1,
                  common.legend = T,
                  labels = c("Power", "", "", "", ""),
                  hjust = -3,
                  vjust = -2.5,
                  font.label = list(
                    face = "plain",
                    size = 9
                  ))

ggsave("figures/F4_levels_vs_power.png", height = 1.75, width = 7,
       dpi = 900)


#### 3. Sign and Magnitude Errors ---------------------------------------------

#### 3.a Figure 5 Type S - heatplot -------------------------------------------

simulation$typeS_robust_rec <- ifelse(simulation$typeS_robust == 1, 0,
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
            data = simulation,
            family = binomial(link = "logit"))
summary(modS)

## For Shiny app export 
# coef<- array(coef(modS))
# coef <- data.frame(V1=coef)
# write.csv(coef, "glm_coefs_type_s.csv",row.names = F)

plot_typeS <- function(z = 0.03, l = 2, change = c("delta", "levels")){
  # Input:
  #   - z: expected effect size
  #   - l: number of levels
  #   - change: which of the two is varied, hence labeled in the plot
  # Output:
  #   - ggplot object
  new <- expand.grid(num_respondents = seq(500, 3000, 10),
                     num_tasks = seq(1, 9, 0.1),
                     true_coef = z, 
                     num_lvls = l)
  new$pred_typeS <- predict.glm(modS, newdata = new, type = "response")
  
  p <- ggplot(new, aes(num_respondents, num_tasks, fill = pred_typeS)) +
    geom_raster(interpolate = F) +
    coord_cartesian(expand = FALSE) +
    scale_x_continuous(breaks = c(1000, 2000, 3000),
                       labels = c("1k", "2k", "3k")) +
    scale_y_continuous(breaks = c(1, 3, 5, 7, 9)) +
    scale_fill_gradient2(
      breaks = c(0, 0.02, 0.04, 0.06, 0.08,
                 0.10, 0.12, 0.14, 0.16, 0.18,
                 0.20, 0.22),
      high = "#E16462FF",
      low = "#0D0887FF",
      limit = c(0, 0.22)
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 6),
      axis.title = element_text(size = 8),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 6)
    ) +
    xlab("Respondents") + ylab("Tasks") +
    guides(fill = guide_colourbar(barwidth = 20,
                                  barheight = 0.5,
                                  frame.colour = "black",
                                  frame.linewidth = 1,
                                  ticks.colour = "black",
                                  ticks.linewidth = 1))
  
  if(change == "delta"){
    p <- p +
      annotate("text", x = 2300, y = 8.2, size = 2.5,
               label = paste("delta ==", z), parse = T)
  } else if(change == "levels") {
    p <- p + 
      annotate("text", x = 2200, y = 8.2, size = 2.5,
               label = paste("levels ==", l), parse = T)
  }
  
  return(p)
}

# Plot with different true_coefs, holding levels constant at l = 3
ggpubr::ggarrange(plot_typeS(z = 0.01, l = 3, change = "delta") + xlab(""),
                  plot_typeS(z = 0.02, l = 3, change = "delta") + xlab("") + ylab(""),
                  plot_typeS(z = 0.03, l = 3, change = "delta") + ylab(""),
                  plot_typeS(z = 0.04, l = 3, change = "delta") + xlab("") + ylab(""),
                  plot_typeS(z = 0.05, l = 3, change = "delta") + xlab("") + ylab(""), 
                  ncol = 5, nrow = 1,
                  common.legend = T,
                  labels = c("Type S", "", "", "", ""),
                  hjust = -2,
                  vjust = -2.5,
                  font.label = list(
                    face = "plain",
                    size = 9
                  ))

ggsave("figures/F5_typeS_effectsize.png", height = 1.75, width = 6.5,
       dpi = 900)


# Plot with different true_coefs
ggpubr::ggarrange(plot_typeS(z = 0.03, l = 2, change = "levels") + xlab(""),
                  plot_typeS(z = 0.03, l = 5, change = "levels") + xlab("") + ylab(""),
                  plot_typeS(z = 0.03, l = 10, change = "levels") + ylab(""),
                  plot_typeS(z = 0.03, l = 15, change = "levels") + xlab("") + ylab(""),
                  plot_typeS(z = 0.03, l = 20, change = "levels") + xlab("") + ylab(""), 
                  ncol = 5, nrow = 1,
                  common.legend = T,
                  labels = c("Type S", "", "", "", ""),
                  hjust = -2,
                  vjust = -2.5,
                  font.label = list(
                    face = "plain",
                    size = 9
                  ))

ggsave("figures/F5_typeS_levels.png", height = 1.75, width = 6.5,
       dpi = 900)


#### 3.b Figure 6 Type M - heatplot -------------------------------------------

simulation_typeM <- simulation %>%
 mutate(typeM_robust_rec = ifelse(typeM_robust < 0, NA, typeM_robust))

plotline_typeM <- function(z){
  
  simulation_typeM %>%
    filter(true_coef == z) %>%
    group_by(num_respondents, num_tasks, true_coef) %>%
    summarize(typeM_mean = mean(typeM_robust_rec, na.rm = T)) %>%
    mutate(eff_n = num_respondents * num_tasks) %>%
    ggplot(aes(x = eff_n, y = typeM_mean,
               group = true_coef)) +
    geom_line() +
    coord_cartesian(xlim = c(0, 27500),
                    ylim = c(0, 13), expand = FALSE) +
    scale_x_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000),
                       labels = c("0", "5k", "10k", "15k", "20k", "25k")) +
    scale_y_continuous(breaks = c(1, 5, 10, 15),
                       labels = c(TeX("1\\times"), 
                                  TeX("5\\times"),
                                  TeX("10\\times"), 
                                  TeX("15\\times"))) +
    geom_hline(yintercept = 1, linetype = 2) +
    annotate("text", x = 21000, y = 11.5, size = 2.5,
             label = paste("delta ==", z), parse = T) +
    theme_bw() +
    theme(axis.text = element_text(size = 6),
          axis.text.y = element_text(margin = margin(0, -0.25, 0, 0)),
          axis.title = element_text(size = 8),
          legend.position = "none",
          panel.grid = element_blank(),
          plot.margin = margin(3, 0.75, 3, 0.75)
    ) +
    xlab("Effective sample") + ylab("Type M Error") +
    geom_hline(yintercept = 1, linetype = 2)
}

# Plot with different true_coefs
ggpubr::ggarrange(plotline_typeM(0.01) + xlab(""),
                  plotline_typeM(0.02) + xlab("") + ylab(""),
                  plotline_typeM(0.03) + ylab(""),
                  plotline_typeM(0.04) + xlab("") + ylab(""),
                  plotline_typeM(0.05) + xlab("") + ylab(""), 
                  ncol = 5, nrow = 1,
                  common.legend = T,
                  labels = c("Type M", "", "", "", ""),
                  hjust = -2,
                  vjust = -2.5,
                  font.label = list(
                    face = "plain",
                    size = 9
                  ))
ggsave("figures/F6_typeM.png", height = 1.35, width = 6.5,
       dpi = 900)



#### 3. SE, Bias, RMSE  -------------------------------------------------------

bias <- simulation %>%
  mutate(bias = est_coef - true_coef,
         rmse = sqrt(bias ^ 2)) %>%
  group_by(num_respondents, num_tasks, num_lvls, true_coef) %>%
  summarize(se_mean = mean(est_se),
            se_sd = sd(est_se),
            bias_mean = mean(bias),
            bias_sd = sd(bias),
            rmse_mean = mean(rmse),
            rmse_sd = sd(rmse)) %>%
  mutate(eff_sample = num_respondents * num_tasks)

bias %>%
  ggplot(aes(x = true_coef, y = bias_mean, 
             group = factor(eff_sample), color = factor(eff_sample))) +
  geom_smooth(alpha = 0.1, se = F) +
  facet_grid( ~ num_lvls) +
  theme_bw()

bias %>%
  ggplot(aes(x = true_coef, y = bias_sd, 
             group = factor(eff_sample), color = factor(eff_sample))) +
  geom_smooth(alpha = 0.1, se = F) +
  facet_grid( ~ num_lvls) +
  theme_bw()

bias %>%
  ggplot(aes(x = true_coef, y = rmse_mean, 
             group = factor(eff_sample), color = factor(eff_sample))) +
  #geom_point() +
  geom_smooth(alpha = 0.1, se = F) +
  facet_grid( ~ num_lvls)

bias %>%
  ggplot(aes(x = true_coef, y = se_mean, 
             group = factor(eff_sample), color = factor(eff_sample))) +
  #geom_point() +
  geom_smooth(alpha = 0.1, se = F) +
  facet_grid( ~ num_lvls)
