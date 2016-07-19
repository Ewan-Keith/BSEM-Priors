##### analysis code for simualated SEM data
######
######

#### set up ####
library(rstan)
library(dplyr)
library(shinystan)
library(loo)
library(ggmcmc)

setwd("~/GitHub/BSEM Priors")

# set up for parallel chains
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#### prepare data ####
current_data <- read.csv("data/sparse/sem_data_1.csv")

# data for correct baseline specification
stan_dat_correct <- list(N = 200,
                 P = 15,
                 D = 3,
                 min_C = 3,
                 X = as.matrix(current_data)
)

# data for full CL estimation
stan_dat <- list(N = 200,
                    P = 15,
                    D = 3,
                    C = 30,
                    X = as.matrix(current_data)
)

#### fit correct specification model ####

fit_correct <- stan(file = 'models/sem_correct_spec.stan', data = stan_dat_correct, 
               iter = 100, chains = 3, thin = 5)#,
               #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 15))

launch_shinystan(fit_correct)


#### fit small variance normal model ####

fit <- stan(file = 'models/sem_cl_gaussian_reparam.stan', data = stan_dat, 
               iter = 1000, chains = 1, thin = 5)

launch_shinystan(fit)

#### fit horseshoe model ####

fit_hs <- stan(file = 'models/sem_cl_hs_reparam.stan', data = stan_dat, 
               iter = 1000, chains = 3, thin = 5,
               control = list(adapt_delta = 0.99))

launch_shinystan(fit_hs)


#### fit lasso model ####

fit_lasso <- stan(file = 'models/sem_cl_lasso_reparam.stan', data = stan_dat, 
               iter = 2000, chains = 3, thin = 5)#,
               #control = list(adapt_delta = 0.999, stepsize = 0.001, max_treedepth = 15))

launch_shinystan(fit_lasso)

#### fit horseshoe plus model ####

fit_hs_plus <- stan(file = 'models/sem_cl_hs+_reparam.stan', data = stan_dat, 
                  iter = 1000, chains = 3, thin = 5,
                  control = list(adapt_delta = 0.999)) #, stepsize = 0.001, max_treedepth = 15))

launch_shinystan(fit_hs_plus)


#### playing with vb ####

basic_model <- stan_model(file = 'models/sem_cl_gaussian.stan')

test <- vb(basic_model, data = stan_dat)

launch_shinystan(test)



hs_model <- stan_model(file = 'models/sem_cl_hs.stan')

test_hs <- vb(hs_model, data = stan_dat)

launch_shinystan(test_hs)


#### playing with WAIC and LOO ####

fit_test <- stan(file = 'models/log_lik_test.stan', data = stan_dat_correct, 
                 iter = 1500, chains = 1, thin = 2)

launch_shinystan(fit_test)

test_extract <- extract_log_lik(fit_test)

test_loo <- loo(test_extract)
test_waic <- waic(test_extract)
