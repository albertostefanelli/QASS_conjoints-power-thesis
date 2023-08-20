###############################################################################
# Power analysis for Conjoint Experiments: People vs. Trials                  #
# Authors: Alberto Stefanelli & Martin Lukac                                  #
# Functions to run the simulation in parallel                                 #
###############################################################################

rm(list=ls(all=TRUE))
source("script_simulation/CJpower_functions.R")

#### list of function loaded from CJpower_functions ####
# lsf.str()

#evaluate.model : function (data, model_formula, true_coefs)              
# generate_design : function (n_attributes, n_levels, n_profiles = 2, rem.eq.prof = T)                                                              
# generate_sample : function (design, units, n_tasks)                      
# get_CL_vcov : function (model, cluster)                                  
# ipak : function (pkg)                                                    
# run_simulation : function (n_attributes, n_levels, n_profiles = 2, rem.eq.prof = T, 
#     units, n_tasks, true_coefs, sigma.u_k, mod_formula)  
# simulate_conjoint : function (data, coef, sigma.u_k = 0.02, LOG = F)  

### Parallel packages
library(doParallel)
library(pbapply)
library(purrr)
library(tidyr)


# Master switch for running the analysis
# -----------------------------------------------------------------------------
  run_simulation_switch <- TRUE # <- TRUE
# -----------------------------------------------------------------------------

# Setup -----------------------------------------------------------------------
# Simulation setup
sim_runs <- 1000
num_clusters <- detectCores()

# Sample generation variables 
sample_size <- c(seq(500, 2000, 250), 3000)
#sample_size <- c(500)
trials <- c(1, 3, 5, 7, 9)
#trials <- c(1)

# Conjoint setup variables
n_attributes <- 5
n_levels <- map(c(2, 3, 4, 5, 10, 15, 20), 
                ~ as.numeric(c(paste(.x), 2, 2, 2, 2)))
effect_size <- c(0.01, 0.02, 0.03, 0.04, seq(0.05, 0.25, 0.05))
sigma.u_k <- 0.02 #seq(0.01, 0.05, 0.01)

# Combine all inputs in a full factorial design
simcombs <- expand_grid(sample_size, trials, 
                        n_attributes, n_levels, 
                        effect_size, sigma.u_k)


# Simulation implementation ---------------------------------------------------

# Prepare a dataframe for simulation results
results_combined <- data.frame(num_respondents=NA,
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


# ----------------------------------- #
# -- WARNING -- WARNING -- WARNING -- #
# --  It takes a long time to run  -- #
# ----------------------------------- #

# Run the simulation
if(run_simulation_switch){
  # For every simulation scenario
  for(i in 1:dim(simcombs)[1]){
    # Set up cluster
    cl <- makeCluster(num_clusters)
    registerDoParallel(cl)
    clusterExport(cl, list("run_simulation", "generate_design", 
                           "generate_sample", "simulate_conjoint", 
                           "evaluate.model", "simcombs", "i","get_CL_vcov" ))
    clusterEvalQ(cl, library("tidyverse"))
    
    # Run
    
    result <- pbreplicate(n = sim_runs, 
                          run_simulation(
                            n_attributes = simcombs$n_attributes[i], 
                            n_levels = unlist(simcombs$n_levels[i]),
                            units = simcombs$sample_size[i], 
                            n_tasks = simcombs$trials[i],
                            true_coefs = rep(simcombs$effect_size[i], 
                                             sum(unlist(simcombs[i,]$n_levels))
                                             - simcombs$n_attributes[i]),
                            sigma.u_k = simcombs$sigma.u_k[i]),
                          cl = cl)
    
    # Stop cluster
    stopCluster(cl)
    
    # Print a msg
    message("===== Iteration ", i, " out of ", dim(simcombs)[1], " =====")
    
    
    df_res <- as.data.frame(t(unlist(result[,,1])))
    for(i in 2:sim_runs){
      df_res[i, ] <- unlist(result[,,i])
    }
    
    results_combined <- rbind(results_combined, df_res)
    
    
  }
}


rio::export(results_combined, "simulation_results_combined.csv")


