###############################################################################
# Power analysis for Conjoint Experiments: People vs. Trials                  #
# Authors: Alberto Stefanelli & Martin Lukac                                  #
# Visualization of the literature review dataset                             #
###############################################################################

#### 0. Libraries and Load in -------------------------------------------------

library(tidyverse)
library(ggpubr)
library(viridis)
library(latex2exp)

literature_review <-read_delim(here("literature-review","lit_review_metadata.csv"), 
                               ";", escape_double = FALSE, 
                               locale = locale(encoding = "UTF-8"), 
                               trim_ws = TRUE) %>%
  select(n_levels_biggests_attribute, n_sample,tot_attributes, tasks,coef) %>%
  mutate(coef = abs(coef)) %>% 
  mutate_at(vars(n_levels_biggests_attribute,n_sample,tot_attributes,tasks,coef),as.numeric)
  
  

samplesize <- data.frame(
  n_levels_biggests_attribute = literature_review$n_levels_biggests_attribute,
  sample_size  = literature_review$n_sample,
  tasks  = literature_review$tasks
)

attributes <- data.frame(
  V1 = literature_review$tot_attributes
)


samplesize %>%
  mutate(total_sample = sample_size * tasks) -> samplesize


## Effect sizes
(eff <- literature_review %>%
    select(coef) %>%
    reshape2::melt() %>%
    ggplot(aes(x = value, y = ..density..)) +
    geom_density(alpha = 0.2, adjust = 1.5,
                 fill = viridis::plasma(1, begin = 0.3),
                 color = viridis::plasma(1, begin = 0.3)) +
    theme_bw() +
    theme(
      legend.position = c(0.7, 0.85),
      legend.key.size = unit(0.25, "cm"),
      legend.title = element_blank(),
      legend.background = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 7),
      axis.title.x = element_text(size = 8),
      panel.grid = element_blank()
    ) +
    ylab("") + 
    xlab(latex2exp::TeX("Estimated AMCE ($\\delta$)")) +
    geom_rug(aes(x = value), sides = "b", inherit.aes = F,
             alpha = 0.8)
)


## Levels
(lev <- samplesize %>%
    ggplot(aes(x = n_levels_biggests_attribute, y = ..density..)) +
    geom_density(adjust = 2, 
                 alpha = 0.5,
                 color = viridis::plasma(1, begin = 0.5),
                 fill = viridis::plasma(1, begin = 0.5)) +
    theme_bw() +
    theme(
      legend.position = c(0.6, 0.6),
      legend.title = element_blank(),
      legend.background = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 7),
      axis.title.x = element_text(size = 8),
      panel.grid = element_blank()
    ) +
    ylab("") + xlab("Largest att. # of levels")+
    geom_rug(aes(x = n_levels_biggests_attribute + 
                   rnorm(length(n_levels_biggests_attribute),
                                   0, 0.1)), 
             sides = "b", inherit.aes = F,
             alpha = 0.8))


max(samplesize$sample_size)

## Sample
(resp <- samplesize %>%
    ggplot(aes(x = sample_size, y = ..density..)) +
    geom_density(adjust = 2,
                 alpha = 0.4,
                 fill = viridis::plasma(1, begin = 0.85),
                 color = viridis::plasma(1, begin = 0.85)) +
    theme_bw() +
    theme(
      legend.position = c(0.6, 0.6),
      legend.title = element_blank(), 
      legend.background = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 7),
      axis.title.x = element_text(size = 8),
      panel.grid = element_blank()
    ) +
    ylab("") + 
    xlab(latex2exp::TeX("Respondents ($n$)")) +
        scale_x_continuous(breaks = c(0, 2000, 4000, 6000, 8000, 
                                  10000, 12000),
                       labels = c("0", "2k", "4k", "6k", "8k", 
                                  "10k", "12k")) +
    coord_cartesian(xlim = c(0, 14000)) +
    geom_rug(aes(x = sample_size), sides = "b", inherit.aes = F,
             alpha = 0.8))


(task <- samplesize %>%
    ggplot(aes(x = tasks, y = ..density..)) +
    geom_density(adjust = 1.5, 
                 alpha = 0.4,
                 fill = viridis::plasma(1, begin = 0.85),
                 color = viridis::plasma(1, begin = 0.85)) +
    theme_bw() +
    theme(
      legend.position = c(0.6, 0.6),
      legend.title = element_blank(),
      legend.background = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 7),
      axis.title.x = element_text(size = 8),
      panel.grid = element_blank()
    ) +
    ylab("") + 
    xlab(latex2exp::TeX("Tasks ($t$)")) +
        scale_x_continuous(breaks = c(1, 2, 4, 6, 8, 10)) +
    geom_rug(aes(x = tasks + rnorm(length(tasks), 0, 0.1)), 
             sides = "b", inherit.aes = F,
             alpha = 0.8) +
    coord_cartesian(xlim = c(0, 11)))

(effsamp <- samplesize %>%
    ggplot(aes(x = total_sample, y = ..density..)) +
    geom_density(adjust = 2, 
                 alpha = 0.4,
                 fill = viridis::plasma(1, begin = 0.8),
                 color = viridis::plasma(1, begin = 0.8)) +
    theme_bw() +
    theme(
      legend.position = c(0.6, 0.6),
      legend.title = element_blank(),
      legend.background = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 7),
      axis.title.x = element_text(size = 8),
      panel.grid = element_blank()
    ) +
    ylab("") + 
    xlab(latex2exp::TeX("$n_{eff} =$ respondents ($n$) $\\times$ tasks ($t$)")) +
    scale_x_continuous(breaks = c(0, 10000, 20000, 30000, 40000, 50000),
                       labels = c("0", "10k", "20k", "30k", "40k", "50k"))+
    coord_cartesian(xlim = c(0, 55000)) +
    geom_rug(aes(x = total_sample), sides = "b", inherit.aes = F,
             alpha = 0.8))


# (scat <- samplesize %>%
#     ggplot(aes(x = sample_size, y = tasks, 
#                color = total_sample, fill = total_sample)) +
#     geom_point(size = 0.9) +
#     theme_bw() +
#     theme(
#       panel.grid = element_blank(),
#       axis.text = element_text(size = 7),
#       axis.title = element_text(size = 8),
#       legend.position = c(0.85, 0.8),
#       legend.title = element_text(size = 7, vjust = 0.9),
#       legend.text = element_text(size = 6, angle = 0, vjust = 0.7),
#       legend.background = element_blank(),
#       legend.key.size = unit("0.3", "cm"),
#       legend.direction = "vertical"
#     )  +
#     scale_x_continuous(breaks = c(0, 2000, 4000, 6000, 8000, 
#                                   10000, 12000, 14000),
#                        labels = c("0", "2k", "4k", "6k", 
#                                   "8k", "10k", "12k", "14k")) +
#     scale_y_continuous(breaks = c(0, 4, 8, 12)) +
#     coord_cartesian(xlim = c(0, 14000), ylim = c(0, 13)) +
#     ylab("Tasks") + xlab("Respondents") +
#     scale_fill_viridis_c(option = "plasma", direction = -1,
#                          end = 0.9, breaks = c(0, 10000, 20000, 
#                                                30000, 40000, 50000),
#                          labels = c("0", "10k", "20k", "30k", "40k", "50k")) +
#     scale_color_viridis_c(option = "plasma", direction = -1,
#                           end = 0.9, breaks = c(0, 10000, 20000, 
#                                                 30000, 40000, 50000),
#                           labels = c("0", "10k", "20k", "30k", "40k", "50k")) +
#     labs(fill = latex2exp::TeX("$n_{eff}$"), 
#          color = latex2exp::TeX("$n_{eff}$")))

(scat <- samplesize %>%
    ggplot(aes(x = sample_size, y = tasks, 
               color = total_sample)) +
    geom_point(size = 0.9) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 8),
      legend.position = c(0.85, 0.8),
      legend.title = element_text(size = 7, vjust = 0.9),
      legend.text = element_text(size = 6, angle = 0, vjust = 0.7),
      legend.background = element_blank(),
      legend.key.size = unit("0.3", "cm"),
      legend.direction = "vertical"
    )  +
    scale_x_continuous(breaks = c(0, 2000, 4000, 6000, 8000, 
                                  10000, 12000, 14000),
                       labels = c("0", "2k", "4k", "6k", 
                                  "8k", "10k", "12k", "14k")) +
    scale_y_continuous(breaks = c(0, 4, 8, 12)) +
    coord_cartesian(xlim = c(0, 14000), ylim = c(0, 13)) +
    ylab("Tasks") + 
    xlab(latex2exp::TeX("Respondents ($n$)")) +
    ylab(latex2exp::TeX("Tasks ($t$)")) +
    scale_color_viridis_c(option = "plasma", direction = -1,
                          end = 0.9, breaks = c(0, 10000, 20000, 
                                                30000, 40000, 50000),
                          labels = c("0", "10k", "20k", "30k", "40k", "50k")) +
    labs(color = latex2exp::TeX("$n_{eff}$")))

ggarrange(ggarrange(resp, task, scat, effsamp, ncol = 2, nrow = 2,
                    labels = c("(a1)", "(a2)", "(a3)", "(a4)"),
                    hjust = 0, vjust = 1,
                    font.label = list(size = 7.5)),
          NULL,
          ggarrange(eff, lev, nrow = 2,
                    labels = c("(b)", "(c)"),
                    hjust = 0, vjust = 1,
                    font.label = list(size = 7.5)), 
          ncol = 3, nrow = 1, widths = c(2, 0.2, 1))

ggsave("figures/F1_metadata_visualisation.png", height = 3.5, width = 6,
       dpi = 900)

