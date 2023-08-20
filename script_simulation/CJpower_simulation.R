###############################################################################
# Power analysis for Conjoint Experiments: People vs. Trials                  #
# Authors: Alberto Stefanelli & Martin Lukac                                  #
# Functions to debug the simulation                                           #
###############################################################################

rm(list=ls(all=TRUE))
source("script_simulation/CJpower_functions.R")
# 1. Inputs -------------------------------------------------------------------
# Sample variables
sample_size <- c(seq(500, 2000, 250), 3000)
trials <- c(1, 3, 5, 7, 9)

# Conjoint variables
## Attributes in the conjoint
n_attributes <- 5

## Number of levels per each attribute (numeric vector of length n_attributes)
n_levels <- map(c(2, 3, 4, 5, 10, 15, 20), 
                ~ as.numeric(c(paste(.x), 2, 2, 2, 2)))

## True effect size
effect_size <- c(0.01, 0.02, 0.03, 0.04, seq(0.05, 0.25, 0.05))

## Treatment effect heterogeneity
sigma.u_k <- 0.02

# Combine in a full factorial design
simcombs <- expand_grid(sample_size, trials, 
                        n_attributes, n_levels, 
                        effect_size, sigma.u_k)

# 2. ~~ Possible workflow ~~ --------------------------------------------------

# A. Set up simulation parameters
# Conjoint setup
## Attributes in the conjoint
n_attributes <- 5

## Number of levels per each attribute (numeric vector of length n_attributes)
n_levels = c(20, 2, 2, 2, 2)

## True effect size (numeric vector of length sum(n_levels) - n_attributes)
simulation_coefs <- c(rep(0.05, 19), 0.02, 0.00, -0.02, -0.05)

# B. Power simulation setup
sim_runs <- 1000
sample_size <- c(200)
num_tasks <- c(3)
sigma <- 0.02

# C. Results grid
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
sim_design <- generate_design(n_profiles = n_profiles, 
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

head(results)

# Wooo! :)
beepr::beep("fanfare")
