###############################################################################
# Power analysis for Conjoint Experiments: People vs. Trials                  #
# Authors: Alberto Stefanelli                                                 #
# Visualization of growth of conjoint experiments in polsci                   #
###############################################################################

library(ggplot2)
library(jtools)
library(dplyr)

# data from dimensions.ai,
year <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016" , "2017", "2018", "2019", "2020", "2021", "2022")
n_citation <- c("18", "22", "33", "43", "47", "57", "76", "100", "160", "189", "254", "290", "323")

cj_citations <- cbind(year,n_citation) |> data.frame() |> mutate_all(as.numeric)

ggplot(data=cj_citations, aes(x=year, y=n_citation)) +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks = seq(min(cj_citations[["year"]]), max(cj_citations[["year"]],1))) +
  geom_text(aes(label=n_citation), vjust=-1) +
  geom_text(aes(x=2013, label="Hainmueller et al. Conjoint Paper", y=200), colour="black", angle=90, vjust = -0.5) +
  geom_vline(xintercept = 2013, linetype=2) + 
  jtools::theme_nice() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))  +
  ylab("Number of Published Articles") + 
  xlab("Year") 

ggsave("figures/n_citations.png", width = 30, height = 20, units = "cm")


