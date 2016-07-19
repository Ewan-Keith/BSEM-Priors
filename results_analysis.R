#### analysis of BSEM simulation results
library(dplyr)
library(reshape2)
library(ggplot2)
library(GGally)
library(ggthemes)
library(lme4)
library(sjPlot)
library(arm)

# preset sjp options
sjp.setTheme(base = theme_tufte())


#### sparse case ####

# read data
sparse_gauss <- read.csv("sim_results/sparse_gaus.csv")
sparse_hs <- read.csv("sim_results/sparse_hs.csv")
sparse_hs_plus <- read.csv("sim_results/sparse_hs_plus.csv")
sparse_no_cl <- read.csv("sim_results/sparse_no_cl.csv")

# adjust for strange adjsutment you made during the simulations
sparse_gauss <- sparse_gauss %>% 
  mutate(cl_6_discrepancy = (.6 - cl_6_discrepancy),
         cl_15_discrepancy = (.2 - cl_15_discrepancy),
         cl_26_discrepancy = (.4 - cl_26_discrepancy))

sparse_hs <- sparse_hs %>% 
  mutate(cl_6_discrepancy = (.6 - cl_6_discrepancy),
         cl_15_discrepancy = (.2 - cl_15_discrepancy),
         cl_26_discrepancy = (.4 - cl_26_discrepancy))

sparse_hs_plus <- sparse_hs_plus %>% 
  mutate(cl_6_discrepancy = (.6 - cl_6_discrepancy),
         cl_15_discrepancy = (.2 - cl_15_discrepancy),
         cl_26_discrepancy = (.4 - cl_26_discrepancy))


#### produce long data for set of plots ####

long_gauss_dens <- melt(sparse_gauss[,1:30]) %>% 
  group_by(variable) %>% 
  summarise(med = median(value),
            low_025 = quantile(value, .025),
            high_975 = quantile(value, .975))

long_hs_dens <- melt(sparse_hs[,1:30]) %>% 
  group_by(variable) %>% 
  summarise(med = median(value),
            low_025 = quantile(value, .025),
            high_975 = quantile(value, .975))

long_hs_plus_dens <- melt(sparse_hs_plus[,1:30]) %>% 
  group_by(variable) %>% 
  summarise(med = median(value),
            low_025 = quantile(value, .025),
            high_975 = quantile(value, .975))


## add on population values for non-zero parameters

pop_cl <- c(rep(NA, 5), 
            0.6, 
            rep(NA, 8),
            0.2,
            rep(NA, 10),
            0.4,
            rep(NA, 4))

long_gauss_dens <- cbind(long_gauss_dens, pop_cl)
long_hs_dens <- cbind(long_hs_dens, pop_cl)
long_hs_plus_dens <- cbind(long_hs_plus_dens, pop_cl)

#### forest plots of median coefficient estimates ####

ggplot(long_gauss_dens, aes(x = med, y = variable)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = long_gauss_dens$low_025, 
                xmax = long_gauss_dens$high_975,
                height = 0)) +
  theme_tufte() +
  xlim(c(-1, 1)) +
  geom_point(aes(x = pop_cl), shape = 5, size = 3) +
  geom_vline(xintercept = c(-1, -.5, 0, .5, 1), colour="grey") +
  ggtitle("Sparse Gauss Density Plot")


ggplot(long_hs_dens, aes(x = med, y = variable)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = long_hs_dens$low_025, 
                     xmax = long_hs_dens$high_975,
                     height = 0)) +
  theme_tufte() +
  xlim(c(-1, 1)) +
  geom_point(aes(x = pop_cl), shape = 5, size = 3) +
  geom_vline(xintercept = c(-1, -.5, 0, .5, 1), colour="grey") +
  ggtitle("Sparse horseshoe Density Plot")


ggplot(long_hs_plus_dens, aes(x = med, y = variable)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = long_hs_plus_dens$low_025, 
                     xmax = long_hs_plus_dens$high_975,
                     height = 0)) +
  theme_tufte() +
  xlim(c(-1, 1)) +
  geom_point(aes(x = pop_cl), shape = 5, size = 3) +
  geom_vline(xintercept = c(-1, -.5, 0, .5, 1), colour="grey") +
  ggtitle("Sparse Horseshoe Plus Density Plot")

#### out of sample model fit comparison ####

# prepare LOO fit data
sparse_fit <- data.frame(no_cl = sparse_no_cl  %>% select(elpd_loo), 
                         gauss = sparse_gauss  %>% select(elpd_loo),
                         hs = sparse_hs  %>% select(elpd_loo),
                         hs_plus = sparse_hs_plus  %>% select(elpd_loo))

# name columns
names(sparse_fit) <- c("no_cl", "gauss", "hs", "hs_plus")

# model number for multi-level model
sparse_fit$model_num <- 1:100

# make long for use and put values on IC scale(*-2)
sparse_fit_long <- sparse_fit %>% 
  melt(id.vars = 'model_num') %>% 
  mutate(value = value * -2)

# multi-level model testing difference in fit
basic_loo_model <- lmer(value ~ 1 + (1 | model_num), data = sparse_fit_long)
test_loo_model <- lmer(value ~ 1 + variable + (1 | model_num), data = sparse_fit_long)

# consistent difference in fit by models
anova(basic_loo_model, test_loo_model)

# extract fixed effect values and SEs for model fit chart

model_fit <- fixef(test_loo_model)
model_fit_se <- se.fixef(test_loo_model)

model_fit_plot <- as.data.frame(cbind(model_fit, model_fit_se))
model_fit_plot$models <- row.names(model_fit_plot)
model_fit_plot <- model_fit_plot %>% filter(models != "(Intercept)")

# plot model fit
ggplot(model_fit_plot, aes(x = models, y = model_fit)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = model_fit_plot$model_fit - 
                      1.96*model_fit_plot$model_fit_se,
                    ymax = model_fit_plot$model_fit + 
                      1.96*model_fit_plot$model_fit_se),
                width = .2) +
  coord_flip() +
  theme_tufte() +
  geom_hline(yintercept = c(-60, -40, -20, 0), colour="grey") +
  ylim(c(-60, 0)) +
  ggtitle("Sparse Model LOO Effect Sizes")


#### LOO parameter number analysis ####

# prepare p_LOO fit data
sparse_param_num <- data.frame(no_cl = sparse_no_cl  %>% select(p_loo), 
                         gauss = sparse_gauss  %>% select(p_loo),
                         hs = sparse_hs  %>% select(p_loo),
                         hs_plus = sparse_hs_plus  %>% select(p_loo))

# name columns
names(sparse_param_num) <- c("no_cl", "gauss", "hs", "hs_plus")

# model number for multi-level model
sparse_param_num$model_num <- 1:100

# make long for use 
sparse_param_long <- sparse_param_num %>% 
  melt(id.vars = 'model_num')

# multi-level model testing difference in number of parameters
basic_param_model <- lmer(value ~ -1 + (1 | model_num), data = sparse_param_long)
test_param_model <- lmer(value ~ -1 + variable + (1 | model_num), data = sparse_param_long)

# consistent difference in fit by models
anova(basic_param_model, test_param_model)

# extract fixed effect values and SEs for bar chart

param_numbers <- fixef(test_param_model)
param_numbers_se <- se.fixef(test_param_model)

param_number_plot <- as.data.frame(cbind(param_numbers, param_numbers_se))
param_number_plot$models <- row.names(param_number_plot)

# plot parameter numbers
ggplot(param_number_plot, aes(x = models, y = param_numbers)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = param_number_plot$param_numbers - 
                       1.96*param_number_plot$param_numbers_se,
                     ymax = param_number_plot$param_numbers + 
                       1.96*param_number_plot$param_numbers_se),
                width = .2) +
  coord_flip() +
  theme_tufte() +
  geom_hline(yintercept = c(420, 430, 440, 450), colour="grey") +
  ylim(c(420, 455)) +
  ggtitle("Sparse Model - Estimated Number of Parameters")








