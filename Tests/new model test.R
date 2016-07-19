library(blavaan)
library(rjags)
library(dplyr)

# //////////////////////////////////////////////////
#### Use blavaan to produce initial model code ####
# //////////////////////////////////////////////////

# initial cfa model code 
HS.model <-
  '
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'

data(HolzingerSwineford1939) # call the HS dataset

# prep data
prep_data <- HolzingerSwineford1939 %>% 
  select(x1, x2, x3, x4, x5, x6, x7, x8, x9)

for(i in 1:ncol(prep_data)){
  prep_data[,i] <- scale(prep_data[,i])
}

# fit the model and save underlying JAGS code
fit <- bcfa(HS.model, 
            data=prep_data,
            std.lv = T,
            jagfile = T)

# //////////////////////////////////////////////////
#### Fit basic JAGS model (no cross loadings) ####
# //////////////////////////////////////////////////

load("lavExport/semjags.rda")

test <-jags.model("lavExport/sem.jag",
                  data = jagtrans$data,
                  n.chains = 1)

thesamps <- coda.samples(test,c('lambda'),
                         n.iter=50000,
                         thin=5,
                         progress.bar="text")

# //////////////////////////////////////////////////
#### Fit cross loading JAGS model - priors = n(0, 100) ####
# //////////////////////////////////////////////////

cltest <-jags.model("lavExport/test_changes.jag",       
                  data = jagtrans$data,
                  n.chains = 1)

theclsamps <- coda.samples(cltest,c('lambda'),
                         n.iter=200000,
                         thin=10,
                         progress.bar="text")

# //////////////////////////////////////////////////
#### Fit cross loading STAN model - priors = n(0, 100) ####
# //////////////////////////////////////////////////

# clear all the above if desired 
# rm(list = ls())

data <- read.csv("data/sem_data_1.csv") %>% select(-X)








