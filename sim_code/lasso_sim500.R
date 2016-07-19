#### Code for carrying out fitting and evaluation of
### laplace priors on cross-loadings
### in SEM
####

library(rstan)
library(loo)
library(dplyr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#///////////////////////////////////////////////////////////////////////////
#### sparse model ####
#//////////////////////////////////////////////////////////////////////////


# define population cross loading values for later comparison
pop_cl <- c(rep(0, 5), 
            0.6, 
            rep(0, 8),
            0.2,
            rep(0, 10),
            0.4,
            rep(0, 4))


# initialise dataframe to hold results
sparse_lasso <- data.frame(
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
  p_loo_se = NA,
  max_rhat = NA
)

n_data <- 1 # number of datasets to simulate

for(i in 1:n_data){
   
  set.seed(456789)
  
  # read in data
  filename <- paste0("data/sparse/sem_data_", i, "_500.csv")
  current_data <- read.csv(eval(filename))
  
  # prep data for stan
  stan_dat <- list(N = 500,
                   P = 15,
                   D = 3,
                   C = 30,
                   X = as.matrix(current_data)
  )
  
  # fit model
  fit <- stan(file = 'models/sem_cl_lasso_reparam.stan', data = stan_dat, 
              iter = 2000, chains = 3, thin = 5)
  
  # extract cross loading samples
  cross_loadings <- rstan::extract(fit, pars = "cl")$cl
  
  # calculate posterior median
  estimated_cl <- apply(cross_loadings, 2, median)
  
  # calculate approximation to population value
  pop_dif <- pop_cl - estimated_cl
  
  # save parameter values
  sparse_lasso[i, 1:30] <- estimated_cl
  temp_errors <- (sparse_lasso[i,1:30] - pop_cl)  %>% 
    as.numeric()
  
  sparse_lasso[i, "RMSE"] <- sqrt(mean(temp_errors^2))
  
  # extract log-likelihood
  log_lik <- extract_log_lik(fit)
  
  # calculate loo values
  loo_extract <- loo(log_lik)
  
  sparse_lasso[i, "elpd_loo"] <- loo_extract$elpd_loo
  sparse_lasso[i, "elpd_loo_se"] <- loo_extract$se_elpd_loo
  sparse_lasso[i, "p_loo"] <- loo_extract$p_loo
  sparse_lasso[i, "p_loo_se"] <- loo_extract$se_p_loo
  
  # calculate maximum r-hat value (all parameters)
  max_rhat_temp <- summary(fit)$summary[,"Rhat"]   %>% max(na.rm = T)
  sparse_lasso[i, "max_rhat"] <- max_rhat_temp
  
  write.csv(sparse_lasso, "sim_results/sparse_lasso.csv", row.names = FALSE)
  
  }






