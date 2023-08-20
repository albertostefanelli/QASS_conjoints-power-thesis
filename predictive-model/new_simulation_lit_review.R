###############################################################################
# Power analysis for Conjoint Experiments: People vs. Trials                  #
# Authors: Alberto Stefanelli & Martin Lukac                                  #
# This is a script for a new small simulation for a set of parameters that    #
# were outside of the range of the initial simulation.                        #
# this allows us to avoid extrapolation and get more reliable estimates       #    
# in retrospective literature review  (see line 22-24 in retro_power_lit_.R   # 
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

getwd()
# Master switch for running the analysis
# -----------------------------------------------------------------------------
  run_simulation_switch <- TRUE # <- TRUE
# -----------------------------------------------------------------------------

# Setup -----------------------------------------------------------------------
# Simulation setup


literature_review_out_of_bounderies <- readr::read_csv("lit_review_new_simu.csv")

# this is only 100 runs 
sim_runs <- 100
num_clusters <- detectCores()

n_levels <- map(literature_review_out_of_bounderies$n_levels_biggests_attribute, 
                ~ as.numeric(c(paste(.x), 2, 2, 2, 2)))

tc_seq <- seq(0.01, 0.15, 0.01)
true_coef <- rep(tc_seq, each = nrow(literature_review_out_of_bounderies))

simcombs <- tibble(
  num_respondents = rep(as.numeric(literature_review_out_of_bounderies$sample_size_1...7), 
                        times = length(tc_seq)),
  num_tasks = rep(as.numeric(literature_review_out_of_bounderies$Tasks), 
                  times = length(tc_seq)),
  n_levels = rep(n_levels,times = length(tc_seq)),
  n_attributes = rep(5, 
                 times = length(tc_seq)*nrow(literature_review_out_of_bounderies)),
  true_coef = true_coef,
  sigma.u_k = rep(0.02,times = length(tc_seq)*nrow(literature_review_out_of_bounderies)))

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
    #i <- 1
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
                            units = simcombs$num_respondents[i], 
                            n_tasks = simcombs$num_tasks[i],
                            true_coefs = rep(simcombs$true_coef[i], 
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

# explort the new small simulation 
#rio::export(results_combined, "simulation_results_combined_lit_review.csv")


