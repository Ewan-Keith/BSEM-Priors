#### BSEM model of holzinger-swineford data
library(dplyr)
library(rstan)
library(shinystan)
library(loo)
library(reshape2)
library(ggplot2)
library(GGally)
library(ggthemes)
library(lme4)
library(arm)
library(OpenMx)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


#### load and prep data ####
data(HS.ability.data)

# remove unused variables
holz_dat <- HS.ability.data %>% 
  dplyr::select(
    -id, -Gender, -grade, -agey, -agem,
    -paperrev, -flagssub,
    -deduct, -numeric, -problemr, -series, -arithmet)

# split out into schools
grant_dat <- holz_dat %>% filter(school == 'Grant-White')
past_dat <- holz_dat %>% filter(school == 'Pasteur')

# standardise each dataset individually
grant_dat_stand <- scale(grant_dat[,-1])
past_dat_stand <- scale(past_dat[,-1])

# clean work space
rm(HS.ability.data, holz_dat, grant_dat, past_dat) 


#### grant school analysis ####

# prep data for stan
stan_grant_dat <- list(N = 156,
                 P = 19,
                 D = 4,
                 C = 57,
                 X = grant_dat_stand
)

# fit Gaussian model
fit_grant <- stan(file = 'H&S analysis/models/H&S Gaussian.stan', 
                  data = stan_grant_dat, 
                  iter = 3000, chains = 3, thin = 3,
                  control = list(adapt_delta = 0.9))

# fit horseshoe model
fit_grant_hs <- stan(file = 'H&S analysis/models/H&S horseshoe.stan', 
                  data = stan_grant_dat, 
                  iter = 3000, chains = 3, thin = 3,
                  control = list(adapt_delta = 0.999))

# fit horseshoe plus model
fit_grant_hs_plus <- stan(file = 'H&S analysis/models/H&S horseshoe plus.stan', 
                          data = stan_grant_dat, 
                          iter = 3000, chains = 3, thin = 3,
                          control = list(adapt_delta = 0.999,
                                         max_treedepth = 15))

## save models
# saveRDS(fit_grant_hs, file = 'H&S analysis/models/fitted models/fitted H&S horseshoe.rds')
# saveRDS(fit_grant, file = 'H&S analysis/models/fitted models/fitted H&S gaussian.rds')
# saveRDS(fit_grant_hs_plus, file = 'H&S analysis/models/fitted models/fitted H&S horseshoe plus.rds')


