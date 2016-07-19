library(simsem) 

# //////////////////////////////////////////////////
#### specify simsem model ####
# //////////////////////////////////////////////////

# define LY as the factor loading matrix

loading <- matrix(0, 15, 3) 

loading[1:5, 1] <- NA 

loading[6:10, 2] <- NA 

loading[11:15, 3] <- NA 

loadingValues <- matrix(0, 15, 3) 

ind <- 0

for(i in 1:3){
  loadingValues[1 + ind,i] <- 0.8
  
  loadingValues[2 + ind,i] <- 0.7
  
  loadingValues[3 + ind,i] <- 0.6
  
  loadingValues[4 + ind,i] <- 0.5
  
  loadingValues[5 + ind,i] <- 0.4
  
  ind <- ind + 5
}

LY <- bind(loading, loadingValues) 

# define PS as the residual variance-covariance matrix among factors

latentCov <- matrix(NA, 3, 3) 

diag(latentCov) <- 1 

PS <- binds(latentCov,0) # population covariance of factors = 0

# define TE  as the measurement error variance-covariance among indicators

TE <- binds(diag(15)) # residual variances of indicators = 1

# create templates for data generation and analysis using model()

CFA.Model <- model(LY = LY, PS = PS, TE = TE, modelType = "CFA")

# //////////////////////////////////////////////////
#### Simulate Data ####
# //////////////////////////////////////////////////

set.seed(456)

n_data <- 1 # number of datasets to simulate

for(i in 1:n_data){
  
  temp_data <- generate(CFA.Model, n = 200)

  write.csv(temp_data, 
            file = paste0("data/sem_data_", i, ".csv"))
  }


