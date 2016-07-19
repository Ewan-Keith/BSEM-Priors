#### Code for carrying out fitting and evaluation of
### small variance gaussian priors on cross-loadings
### in SEM
####

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
sparse_gaus <- data.frame(
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

for(i in 1:n_data){
  
  set.seed(456789)
   
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
  fit <- stan(file = 'models/sem_cl_gaussian_reparam.stan', data = stan_dat, 
              iter = 1000, chains = 3, thin = 3)
  
  # extract cross loading samples
  cross_loadings <- rstan::extract(fit, pars = "cl")$cl
  
  # calculate posterior median
  estimated_cl <- apply(cross_loadings, 2, median)
  
  # calculate approximation to population value
  pop_dif <- pop_cl - estimated_cl
  
  # save error values
  sparse_gaus[i, 1:30] <- pop_dif
  temp_errors <- sparse_gaus[i,1:30]  %>% 
    as.numeric()
  
  sparse_gaus[i, "RMSE"] <- sqrt(mean(temp_errors^2))
  
  # extract log-likelihood
  log_lik <- extract_log_lik(fit)
  
  # calculate loo values
  loo_extract <- loo(log_lik)
  
  sparse_gaus[i, "elpd_loo"] <- loo_extract$elpd_loo
  sparse_gaus[i, "elpd_loo_se"] <- loo_extract$se_elpd_loo
  sparse_gaus[i, "p_loo"] <- loo_extract$p_loo
  sparse_gaus[i, "p_loo_se"] <- loo_extract$se_p_loo
  
  write.csv(sparse_gaus, "sim_results/sparse_gaus.csv", row.names = FALSE)
  
  }






