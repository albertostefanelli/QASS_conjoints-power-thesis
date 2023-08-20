###############################################################################
# Power analysis for Conjoint Experiments: People vs. Trials                  #
# Authors: Alberto Stefanelli & Martin Lukac                                  #
# Functions to simulate conjoint data given a set of input                    #
###############################################################################

#### 0. Libraries -------------------------------------------------------------
options(repos = c(
  CRAN = 'https://cran.rstudio.com',
  CRANextra = 'https://macos.rbind.io'
))

rm(list=ls(all=TRUE))
library(devtools)
# install and load defined list of packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE,type="binary")
  sapply(pkg, require, character.only = TRUE)
}

list_of_required_pkg <- c(
  "dplyr", 
  "tidyverse",
  "sandwich", 
  "beepr"
)

ipak(list_of_required_pkg)


### --------------------------------###
### Generate FULL FACTORIAL DESIGN  ###
### --------------------------------###

#### 1. Generate full factorial -----------------------------------------------
# n_profiles = 2
# n_attributes = 3
# n_levels = c(2, 2, 2, 2, 2)
# rem.eq.prof = T

generate_design <- function(n_attributes,
                            n_levels,
                            n_profiles = 2,
                            rem.eq.prof = T){
  
  # Generate full factorial experimental design
  # ---------------------------------------------------------------------------
  # Input: n_profiles - int; number of profiles to be compared together
  #        n_attributes - int; number of independent variables (ie treatments)
  #                       in the conjoint experiment
  #        n_levels - vector of ints of length n_attributes; number of levels
  #                   of variables should be > 1.
  #        rem.eq.prof - logical; remove comparisons of same profiles
  #
  # Output: dataframe - large dataframe with a full factorial design, which 
  #                     combines all possible levels together.
  # ---------------------------------------------------------------------------
  
  ### A. Test number of levels input 
  if(length(n_levels) != n_attributes){
    stop("Number of levels is not equal to number of attributes.")
  }
  
  ### B. Test inputs for < 0
  if(n_profiles <= 0 |
     n_attributes <= 0){
    stop("Inputs have to be > 0.")
  }
  
  ### C. Test number of levels > 1
  if(any(n_levels <= 1)){
    stop("Number of levels has to be > 1.")
  }
  
  # Attribute names: var_1 ... var_x
  named_attributes <- rep(paste0("var_",1:n_attributes), each = n_profiles)
  named_attributes <- sort(named_attributes)
  
  # Profile names: Profile_1_var_1 ... Profile_2_var_x
  profile_names <- paste("Profile",1:(n_profiles),named_attributes,sep="_")
  
  # Levels of each var
  att_levels <- rep(n_levels, 1, each = n_profiles)
  
  grid_list <- c()
  for (i in 1:length(profile_names)){
    grid_list[[profile_names[i]]] <- as.numeric(0:(att_levels[i] - 1))
  }
  
  
  design <- expand.grid(grid_list, stringsAsFactors = FALSE)
  

  if(rem.eq.prof){
    # Split
    p1 <- select(design, starts_with("Profile_1"))
    p2 <- select(design, starts_with("Profile_2"))
    
    # Check
    same_profile <- apply((p1 == p2), 
                          1, function(x) all(x))
    
    # Remove
    design <- design[!same_profile, ]
  }
  
  return(design)
}

# Examples:
# design_2_5 <- generate_design(n_profiles = 2,
#                               n_attributes = 5,
#                               n_levels = c(2, 2, 2, 2, 2))
# 
# design_2_2 <- generate_design(n_profiles = 2,
#                               n_attributes = 2,
#                               n_levels = c(2, 2))
# 
# design_2_2 <- generate_design(n_profiles = 2,
#                               n_attributes = 2,
#                               n_levels = c(2, 2))

#### 2. Generate a sample of profiles from the full factorial -----------------

# units = 100
# n_tasks = 3
# design = design_2_5

generate_sample <- function(design,       # dataframe from generate_design()
                            units,        # number of respondents
                            n_tasks){     # number of tasks
  
  # Generate sample of profiles from the full factorial design
  # ---------------------------------------------------------------------------
  # Input: design - dataframe from generate_design()
  #        units - int; number of respondents
  #        n_tasks - number of tasks performed by each respondent
  #
  # Output: dataframe - samples from the full factorial design in dimension
  #                     units * n_tasks
  # ---------------------------------------------------------------------------
  
  # full_factorial <- design
  
  sample <- dplyr::sample_n(design, size = units * n_tasks,
                            replace = TRUE) %>%
    add_column(id = rep(1:units, each = n_tasks), .before = "Profile_1_var_1")
  
  
  return(sample)
}

# Examples
# sample_1000 <- generate_sample(design = design_2_5, units = 1000, n_tasks = 4)
# sample_2_2_1000 <- generate_sample(design = design_2_2, units = 1000, n_tasks = 4)


#### 3. Simulate CJ responses to sampled profiles from the full factorial -----

simulate_conjoint <- function(data,                # from generate_sample()
                              coef,                # vector of coefs to sim
                              sigma.u_k = 0.02,    # treatment heterogeneity
                              LOG = F){            # diagnostics printed
  
  # Simulate conjoint process of profile selection
  # ---------------------------------------------------------------------------
  # Input: data - dataframe with treatment variables generated 
  #               by generate_sample() - i.e. wide conjoint format
  #        coef - a vector of coefficients for all parameters to be estimated
  #               in order: var_1_lvl_1, var_1_lvl2, var_2_lvl1, var_2_lvl2,...
  #        sigma.u_k - standard deviation of the treatment effect for each var
  #        LOG - used for diagnosing bugs in the function: prints checks
  #
  # Output: dataframe - addition of column "y" to data, meaning selection of
  #                     that respective profile - in long conjoint format
  # ---------------------------------------------------------------------------
  
  data_orig <- data
  
  #### Get inputs for later calculations
  num_trials <- dim(filter(data, id == "1"))[1]
  
  # Get number of variables in the conjoint design: num_vars
  num_vars <- length(names(select(data, starts_with("Profile_1"))))
  
  # Get number of levels for each variable in the conjoint design: num_lvls
  data %>%
    select(starts_with("Profile_1")) -> sim_lvls
  num_lvls <- c()
  for(i in 1:num_vars){
    num_lvls[i] <- length(names(table(
      eval(parse(text=(paste0("data$Profile_1_var_", i)))))
    ))
  }
  
  # Get a list of length num_vars and vector of coefs for each (level - 1)
  lvls_list <- list()
  lvls_iterator <- 1
  for(var in 1:num_vars){
    for(lvl in (num_lvls - 1)[var]){
      lvls_list[[var]] <- coef[lvls_iterator]
      if(lvl > 1){
        for(z in 2:lvl){
          lvls_list[[var]] <- append(lvls_list[[var]], coef[lvls_iterator])
          lvls_iterator <- lvls_iterator + 1
        }
      } else {
        lvls_iterator <- lvls_iterator + 1
      }
    }
  }
  rm(sim_lvls)
  
  ### Test if coefficients were parsed correctly
  if(LOG){
    message("========= simulate_conjoint() diagnostic check =========")
    message("========================================================")
    message("= Parsing of coefficients:")
    for(var in 1:num_vars){
      coef_extracted <- paste(lvls_list[[var]], sep = ", ")
      for(z in 1:length(coef_extracted)){
        message("= Variable ", var, " (levels: ", num_lvls[var], 
                "), dummy ", z, "/", num_lvls[var] - 1, ": ", 
                coef_extracted[z])
      }
    }
    message("========================================================")
  }
  
  
  # Calculate number of parameters needed for the simulation
  num_parameters <- sum(num_lvls) - num_vars
  
  
  #### Run basic checks for inputs
  # Check if the input number of parameters is correct
  if(num_parameters > length(coef)){
    warning("      Input problem: Not enough parameters!")
    warning("      --> Expected ", num_parameters, " parameters.")
    break
  }
  
  if(num_parameters < length(coef)){
    warning("      Input problem: Too many parameters!")
    warning("      --> Expected ", num_parameters, " parameters.")
    break
  }
  
  
  ### Initialize calculation of conjoint reponses
  coef_iterator <- 1
  # Set intercept for forced-choice experiment
  data$probability_p1 <- 0.5
  data$probability_p2 <- 0.5
  
  for(var in 1:num_vars){
    for(lvl in (num_lvls - 1)[var]){
      for(z in 1:lvl){
        #### Treatment effect heterogeneity
        # Generate vector of random effects for each variable * (lvls - 1)
        # and calculate the individual effect per each respondent:
        #     indiv_coef_x = true_coef_x + random_effect_u_x
        # Stage 1: Random effect generation
        # calculate a random effect for each variable
        reff_u <- paste0("raneff_u", var, "_lvl", z)
        # add it to the used dataframe
        data[, paste0(reff_u)] <- rep(rnorm(length(unique(data$id)), 
                                            mean = 0, sd = sigma.u_k), 
                                      each = num_trials)
        
        # Stage 2: Effect calculation
        # Calculate respondent specific effect per each variable * (lvl - 1)
        coef_x <- paste0("cx", var, "_lvl", z)
        
        # [resp.-spec. coef]        [average effect]      [random effect]
        data[, paste0(coef_x)] <- coef[coef_iterator] + data[, paste0(reff_u)]
        coef_iterator <- coef_iterator + 1
        
        
        #### Calculate the predicted probability of selecting a profile
        # Linear probability predictor
        coef_x <- paste0("cx", var, "_lvl", z)
        data$probability_p1 <- data$probability_p1 +
          data[, paste0(coef_x)] * 
          ifelse(data[,paste0("Profile_1_var_", var)] == z, 1, 0)
        
        # Debug message:
        # message("Multiplying: P1 = ", coef_x, " * ", "Profile_1_var_", 
        #        var, "(lvl = ", z, ")")
        
        data$probability_p2 <- data$probability_p2 +
          data[, paste0(coef_x)] * 
          ifelse(data[,paste0("Profile_2_var_", var)] == z, 1, 0)
      }
    }
  }
  # Explanation: If we only have one "individual_heterogeneity" term, it
  #              assumes that the heterogeneity is equal across all the
  #              Xs. That is unlikely! We need a random effect for each
  #              independent variable, because each can differ on its own.
  
  data$probability_p1 <- pmin(data$probability_p1, 0.999)
  data$probability_p1 <- pmax(data$probability_p1, 0.001)
  data$probability_p2 <- pmin(data$probability_p2, 0.999)
  data$probability_p2 <- pmax(data$probability_p2, 0.001)
  
  #### Select the winning profile
  # Odds ratio of choosing P1 vs. P2
  # This is the fucking magic sauce I was looking for!
  odds_choose_p1 <- (data$probability_p1 / (1 - data$probability_p1)) /
    (data$probability_p2 / (1 - data$probability_p2))
  
  # Convert to probability
  prob_choose_p1 <- odds_choose_p1 / (1 + odds_choose_p1)
  
  # Generate selections with rbinom()
  selected_p1 <- rbinom(n = length(prob_choose_p1), size = 1, 
                        prob = prob_choose_p1)
  
  # Gather everything into one dataframe
  data_final <- data.frame(data_orig, 
                           probability_p1 = data$probability_p1, 
                           probability_p2 = data$probability_p2,
                           odds_choose_p1_over_p2 = odds_choose_p1,
                           prob_choose_p1_over_p2 = prob_choose_p1,
                           y1 = selected_p1,
                           y2 = ifelse(selected_p1 == 1, 0, 1))
  
  # Add task indicator
  data_final %>%
    group_by(id) %>%
    mutate(task = seq(1, as.numeric(paste(table(data$id)[1])))) -> data_final
  
  # Wide to long format
  # Split into two
  data_final %>% select(id, task, starts_with("Profile_1"), y1) -> p1
  data_final %>% select(id, task, starts_with("Profile_2"), y2) -> p2
  # Rename vars to match
  names(p1)[grep("Profile_", names(p1))] <- paste0("var_",
                                                   (1:(length(
                                                     grep("Profile_",
                                                          names(p1))))))
  names(p1)[grep("y1", names(p1))] <- "y"
  names(p2)[grep("Profile_",names(p2))] <- paste0("var_",
                                                  (1:(length(
                                                    grep("Profile_",
                                                         names(p2))))))
  names(p2)[grep("y2", names(p2))] <- "y"
  
  # Add hypothetical profile ordering: 
  #     left: comparison = 1
  #     right: comparison = 2
  # p1 <- p1 %>% 
  #   add_column(comparison = 1, .before = "var_1")
  # p2 <- p2 %>% 
  #   add_column(comparison = 2, .before = "var_1")
  
  # Combine in one dataframe
  stack <- rbind(p1,p2) %>%
    arrange(id, task)
  
  return(stack)
}

###############################################################################

# Example manual workflow
# sim_design <- generate_design(n_profiles = 2, 
#                 n_attributes = 5,
#                 n_levels = c(2, 2, 2, 2, 2))
# 
# sim_samples <- generate_sample(design = sim_design, units = 100, n_tasks = 5)
# 
# sim_data <- simulate_conjoint(sim_samples,
#                               coef = c(0.02, 0.02, 0.00, -0.02, 0.05),
#                               LOG  = T,
#                               sigma.u_k = 0.05)
# 
# # Linear Probability Model 
# lpm <- glm(y ~  var_1 + var_2 + var_3 + var_4 + var_5,
#            data = sim_data, 
#            family = gaussian()) 
# summary(lpm)

#### 4. Calculate and evaluate measures of power ------------------------------

# Function to get the robust vcov from glm model 
# Also work with lm()
get_CL_vcov <- function(model, cluster){
  #calculate degree of freedom adjustment
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  #calculate the uj's
  uj  <- apply(sandwich::estfun(model),2, function(x) tapply(x, cluster, sum))
  
  #use sandwich to get the var-covar matrix
  vcovCL <- dfc*sandwich::sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
}
 
evaluate.model <- function(data, # from simulate_conjoint 
  model_formula, 
  true_coefs # from a vector of lenght specified in the simulate_conjoint 
){

  # Debugging for the evaluate.model. The sim_cj is taken from CJpower_simulation.R
  #data <- sim_cj
  #true_coefs = c(0.02, 0.02, 0.02, 0.02, 0.02)  

  # names(data)
  # Simulate conjoint process of profile selection
  # ---------------------------------------------------------------------------
  # Input: data - dataframe with treatment variables generated 
  #               by generate_sample() and response by simulate_conjoint(),
  #               i.e. long conjoint format
  #        model_formula - formula for the model to be estimated, by default
  #                        it estimates a gaussian() GLM
  #                      
  #        true_coefs  - a vector of true parameters to compare the result to
  #                     should be same as coef in simulate_conjoint()
  #                    
  #
  # Output: vector of results:
  #             - est_coef: estimated coefficient
  #             - est_se: estimated standard error
  #             - true_coef: coefficient from true_coefs[1]
  #             - sig: logical, if the est_coef was significant (alpha = 0.05)
  #             - in_ci95: logical, if the true_coef was in the CI95
  #             - typeS: logical, did the sign of sig. coef match to true?
  #             - typeM: ratio of estimated/true, if significant
  #                 (for typeS and typeM, see Gelman and Carlin, 2014)
  # ---------------------------------------------------------------------------
  
  # For now, formula is fixed ~ potentially changeable
  model_formula <- y ~ as.factor(var_1) + 
    as.factor(var_2) + 
    as.factor(var_3) +
    as.factor(var_4) + 
    as.factor(var_5)
  
  # Get inputs
  num_respondents <- length(unique(data$id))
  num_tasks <- length(unique(data$task))
  num_attrbs <- sum(str_detect(names(data), "var_"))  # <- will be problem here!
  num_lvls <- length(names(table(data$var_1)))

  data <- data %>% ungroup() %>% 
  mutate_at(vars(starts_with("var_")), factor) 

  ## Linear probability model
  mod <- glm(model_formula, data = data, family = gaussian())
  
  # Retrieve coefficient estimates and standard errors
  est_coefs <- mod$coefficients[-1]
  est_se <- sqrt(diag(vcov(mod)))[-1]

  # get the robust variance covariance matrix
  vcov_sandwich <- get_CL_vcov(mod,data$id)
  
  # squareroot the diagonal to get the SE and remove the intercept se
  est_se_robust <- sqrt(diag(vcov_sandwich))[-1]

  ## Average effect
  est_coef <- est_coefs[1]
  
  ## Average s.e. 
  est_se <- est_se[1]
  
  # get ONLY the robust SE for the first coefficient 
  est_se_robust <- est_se_robust[1]

  ## True coef
  true_coef <- true_coefs[1]

  # Check whether the estimate has a correct sign: results to T or F
  est_coef_positive <- ifelse(sign(true_coefs[1]) == sign(est_coefs[1]), T, F)
  
  # Check whether the estimate is in CI95: results to T or F
  in_ci95 <- ifelse((true_coefs[1] < est_coefs[1] + 1.96 * est_se[1]) &
                      true_coefs[1] > est_coefs[1] - 1.96 * est_se[1], 
                    T, F)
  in_ci95_robust <- ifelse((true_coefs[1] < est_coefs[1] + 1.96 * est_se_robust[1]) &
                      true_coefs[1] > est_coefs[1] - 1.96 * est_se_robust[1], 
                      T, F)

  # Is significant?
  sig <- ifelse(est_coefs[1] - 1.96 * est_se[1] > 0, T,
                ifelse(est_coefs[1] + 1.96 * est_se[1] < 0, T, F))

  sig_robust <- ifelse(est_coefs[1] - 1.96 * est_se_robust[1] > 0, T,
                ifelse(est_coefs[1] + 1.96 * est_se_robust[1] < 0, T, F))
  
  # If significant, check if 
  typeS <- ifelse(sig, ifelse(sign(est_coefs[1]) == sign(true_coefs[1]),
                              T, F), NA)
  typeS_robust <- ifelse(sig_robust, ifelse(sign(est_coefs[1]) == sign(true_coefs[1]),
                              T, F), NA)
  
  # Type M: overestimation magnitude
  typeM <- ifelse(sig, est_coefs[1] / true_coefs[1], NA)
  
  typeM_robust <- ifelse(sig_robust, est_coefs[1] / true_coefs[1], NA)

  # Combine and return
  result <- cbind.data.frame(num_respondents, 
                             num_tasks,
                             num_attrbs,
                             num_lvls,
                             true_coef, 
                             est_coef,
                             est_se,est_se_robust, 
                             sig,sig_robust,
                             in_ci95,in_ci95_robust,
                             typeS,typeS_robust, 
                             typeM,typeM_robust
                           )
  
  return(result)
}

# Example
# evaluate.model(data = sim_data, 
#                model_formula = y ~ var_1 + var_2 + var_3 + var_4 + var_5, 
#                true_coefs = c(0.02, 0.02, 0.00, -0.02, 0.05))


#### 5. Simulation function for given inputs ----------------------------------

run_simulation <- function(n_attributes, 
                           n_levels, 
                           n_profiles = 2, 
                           rem.eq.prof = T,
                           units, 
                           n_tasks,
                           true_coefs, 
                           sigma.u_k,
                           mod_formula){
  sim_res <- generate_design(n_attributes, n_levels, n_profiles, rem.eq.prof) %>%
    generate_sample(units, n_tasks) %>%
    simulate_conjoint(coef = true_coefs, sigma.u_k) %>%
    evaluate.model(mod_formula, true_coefs)
  return(sim_res)
}
