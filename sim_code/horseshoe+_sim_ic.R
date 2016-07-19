#### Code for carrying out fitting and evaluation of
### laplace priors on cross-loadings
### in SEM
####

library(rstan)
library(dplyr)
library(loo)

# set up for parallel chains
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#///////////////////////////////////////////////////////////////////////////
#### independant model ####
#//////////////////////////////////////////////////////////////////////////

setwd("~/GitHub/BSEM Priors")


# initialise dataframe to hold results
independant_hsplus <- data.frame(
  cl_1_discrepancy = NA, cl_2_discrepancy = NA, cl_3_discrepancy = NA,
  cl_4_discrepancy = NA, cl_5_discrepancy = NA, cl_6_discrepancy = NA,
  cl_7_discrepancy = NA, cl_8_discrepancy = NA, cl_9_discrepancy = NA,
  cl_10_discrepancy = NA, cl_11_discrepancy = NA, cl_12_discrepancy = NA,
  cl_13_discrepancy = NA, cl_14_discrepancy = NA, cl_15_discrepancy = NA,
  cl_16_discrepancy = NA, cl_17_discrepancy = NA, cl_18_discrepancy = NA,
  cl_19_discrepancy = NA, cl_20_discrepancy = NA, cl_21_discrepancy = NA,
  cl_22_discrepancy = NA, cl_23_discrepancy = NA, cl_24_discrepancy = NA,
  cl_25_discrepancy = NA, cl_26_discrepancy = NA, cl_27_discrepancy = NA,
  cl_28_discrepancy = NA, cl_29_discrepancy = NA, cl_30_discrepancy = NA,
  RMSE = NA,
  elpd_loo = NA,
  elpd_loo_se = NA,
  p_loo = NA,
  p_loo_se = NA
)

n_data <- 100 # number of datasets to simulate

set.seed(456789)

for(i in 1:n_data){
  
  # read in data
  filename <- paste0("data/independant/sem_data_", i, ".csv")
  current_data <- read.csv(eval(filename))
  
  # prep data for stan
  stan_dat <- list(N = 200,
                   P = 15,
                   D = 3,
                   C = 30,
                   X = as.matrix(current_data)
  )
  
  # fit model
  fit <- stan(file = 'models/sem_cl_hs+_reparam.stan', data = stan_dat, 
              iter = 1000, chains = 3, thin = 5,
              control = list(adapt_delta = 0.99))
  
  # extract cross loading samples
  cross_loadings <- rstan::extract(fit, pars = "cl")$cl
  
  # calculate posterior median
  estimated_cl <- apply(cross_loadings, 2, median)
  
  # save error values
  independant_hsplus[i, 1:30] <- estimated_cl
  temp_errors <- independant_hsplus[i,1:30]  %>% 
    as.numeric()
  
  independant_hsplus[i, "RMSE"] <- sqrt(mean(temp_errors^2))
  
  # extract log-likelihood
  log_lik <- extract_log_lik(fit)
  
  # calculate loo values
  loo_extract <- loo(log_lik)
  
  independant_hsplus[i, "elpd_loo"] <- loo_extract$elpd_loo
  independant_hsplus[i, "elpd_loo_se"] <- loo_extract$se_elpd_loo
  independant_hsplus[i, "p_loo"] <- loo_extract$p_loo
  independant_hsplus[i, "p_loo_se"] <- loo_extract$se_p_loo
  
  write.csv(independant_hsplus, "sim_results/independant_hsplus.csv", row.names = FALSE)
  
}






