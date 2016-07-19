
library(rstan)
library(dplyr)
setwd("~/GitHub/BSEM Priors")


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## data

library(lavaan)
data("HolzingerSwineford1939")

holz <-HolzingerSwineford1939 %>% filter(complete.cases(HolzingerSwineford1939))
holz <- holz[,-c(1:6)]

s_holz <- scale(holz) %>% as.data.frame()

stan_dat <- list(N = 300,
                 P = 9,
                 D = 3,
                 
                 X = as.matrix(s_holz)
)

fit <- stan(file = 'lavExport/first test.stan', data = stan_dat, 
            iter = 10000, chains = 4, thin = 3)

library(shinystan)
launch_shinystan(fit)

test_fit <- cfa(HS.model, 
                data=s_holz,
                std.lv = T, meanstructure = TRUE)

#### cross loading test #### requires positive constraint on main loadings

stan_cl_dat <- list(N = 300,
                 P = 9,
                 D = 3,
                 C = 18,
                 X = as.matrix(s_holz)
)

cl_fit <- stan(file = 'lavExport/cl test.stan', data = stan_cl_dat, 
            iter = 5000, chains = 7, thin = 3)


###---------------------------------------------
#### using 15 var data now ####
###---------------------------------------------

data <- read.csv("data/sem_data_1.csv") %>% select(-X)

stan_cl_dat <- list(N = 200,
                    P = 15,
                    D = 3,
                    C = 30,
                    X = as.matrix(data)
)

cl_fit <- stan(file = 'models/cl test.stan', data = stan_cl_dat, 
               iter = 5000, chains = 4, thin = 3)

launch_shinystan(cl_fit)




