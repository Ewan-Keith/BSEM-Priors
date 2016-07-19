#### Code for carrying out fitting and evaluation of
### horseshoe+ priors on cross-loadings
### in SEM
####

library(rstan)
library(loo)

# set up for parallel chains
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#///////////////////////////////////////////////////////////////////////////
#### sparse model ####
#//////////////////////////////////////////////////////////////////////////

setwd("~/GitHub/BSEM Priors")

# initialise dataframe to hold results
sparse_no_cl <- data.frame(
    elpd_loo = NA,
  elpd_loo_se = NA,
  p_loo = NA,
  p_loo_se = NA
)

n_data <- 100 # number of datasets to simulate

set.seed(456789)

for(i in 1:n_data){
   
  # read in data
  filename <- paste0("data/sparse/sem_data_", i, ".csv")
  current_data <- read.csv(eval(filename))
  
  # prep data for stan
  stan_dat <- list(N = 200,
                   P = 15,
                   D = 3,
                   C = 30,
                   X = as.matrix(current_data)
  )
  
  # fit model
  fit <- stan(file = 'models/sem_non_cl.stan', data = stan_dat, 
                      iter = 1000, chains = 3, thin = 5)
  
  
  # extract log-likelihood
  log_lik <- extract_log_lik(fit)
  
  # calculate loo values
  loo_extract <- loo(log_lik)
  
  sparse_no_cl[i, "elpd_loo"] <- loo_extract$elpd_loo
  sparse_no_cl[i, "elpd_loo_se"] <- loo_extract$se_elpd_loo
  sparse_no_cl[i, "p_loo"] <- loo_extract$p_loo
  sparse_no_cl[i, "p_loo_se"] <- loo_extract$se_p_loo
  
  write.csv(sparse_no_cl, "sim_results/sparse_no_cl.csv", row.names = FALSE)
  
  }






