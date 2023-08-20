###############################################################################
# Power analysis for Conjoint Experiments: People vs. Trials                  #
# Authors: Alberto Stefanelli & Martin Lukac                                  #
# Functions to debug the simulation                                           #
###############################################################################

rm(list=ls(all=TRUE))
source("script_simulation/CJpower_functions.R")

# Set up simulation parameters
# Number of attributes in the conjoint
n_attributes <- 5
# Number of levels per each attribute (numeric vector of length n_attributes)
n_levels = c(2, 2, 2, 2, 2)
# Hypothesized effect size (numeric vector of length sum(n_levels) - n_attributes)
simulation_coefs <- c(-0.04, 0.02, 0.00, -0.02, -0.05)
# Number of Respondents
sample_size <- c(1000)
# Number of tasks 
num_tasks <- c(3)
# Treatment heterogeneity
sigma <- 0.10
# simulation runs
sim_runs <- 50
# seed for repo
set.seed(999)

# Results grid
results <- data.frame(num_respondents=NA,
                      num_tasks=NA,
                      num_attrbs=NA,
                      num_lvls=NA,
                      true_coef=NA, 
                      est_coef=NA,
                      
                      est_se=NA,
                      est_se_robust=NA, 
                      
                      sig=NA,
                      sig_robust=NA,
                      
                      in_ci95=NA,
                      in_ci95_robust=NA,
                      
                      typeS=NA,
                      typeS_robust=NA,
                      
                      typeM=NA,
                      typeM_robust=NA
)         

# D. Generate full factorial design
sim_design <- generate_design(n_profiles = 2, 
                              n_attributes = n_attributes,
                              n_levels = n_levels)

# E. Run the simulation
for(s in 1:sim_runs){
  sample <- generate_sample(design = sim_design, 
                            units = sample_size, 
                            n_tasks = num_tasks)
  
  
  cj <- simulate_conjoint(data=sample, 
                          coef = simulation_coefs,
                          sigma.u_k = sigma, 
                          LOG = F)
  
  evaluate <- evaluate.model(cj, 
                             y ~ as.factor(var_1) + as.factor(var_2) + as.factor(var_3) + 
                               as.factor(var_4) + as.factor(var_5), 
                             true_coefs = simulation_coefs)
  
  # in case order is not the same between results df and output of the 
  # evaluate function in this script 
  
  names(results) <- names(evaluate) 
  results[s,] <- evaluate
}

mean(results$sig_robust)
mean(results$typeS_robust, na.rm=T)
mean(results$typeM_robust, na.rm=T)

# Wooo! :)
beepr::beep("fanfare")
