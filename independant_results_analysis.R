#### analysis of BSEM simulation results
library(dplyr)
library(reshape2)
library(ggplot2)
library(GGally)
library(ggthemes)
library(lme4)
library(sjPlot)
library(arm)

#### sparse case ####

# read data
ind_gauss <- read.csv("sim_results/independant_gaus.csv")
ind_hs <- read.csv("sim_results/independant_hs.csv")
ind_hs_plus <- read.csv("sim_results/independant_hsplus.csv")
ind_no_cl <- read.csv("sim_results/independant_no_cl.csv")

#### produce long data for set of plots ####

ind_long_gauss_dens <- melt(ind_gauss[,1:30]) %>% 
  group_by(variable) %>% 
  summarise(med = median(value),
            low_025 = quantile(value, .025),
            high_975 = quantile(value, .975))

ind_long_hs_dens <- melt(ind_hs[,1:30]) %>% 
  group_by(variable) %>% 
  summarise(med = median(value),
            low_025 = quantile(value, .025),
            high_975 = quantile(value, .975))

ind_long_hs_plus_dens <- melt(ind_hs_plus[,1:30]) %>% 
  group_by(variable) %>% 
  summarise(med = median(value),
            low_025 = quantile(value, .025),
            high_975 = quantile(value, .975))

#### forest plots of median coefficient estimates ####

ggplot(ind_long_gauss_dens, aes(x = med, y = variable)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = ind_long_gauss_dens$low_025, 
                     xmax = ind_long_gauss_dens$high_975,
                     height = 0)) +
  theme_tufte() +
  xlim(c(-.5, .5)) +
  geom_vline(xintercept = c(-.5, -.25, 0, .25, .5), colour="grey") +
  ggtitle("Independant Gauss Density Plot")


ggplot(ind_long_hs_dens, aes(x = med, y = variable)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = ind_long_hs_dens$low_025, 
                     xmax = ind_long_hs_dens$high_975,
                     height = 0)) +
  theme_tufte() +
  xlim(c(-.5, .5)) +
  geom_vline(xintercept = c(-.5, -.25, 0, .25, .5), colour="grey") +
  ggtitle("Independant Horseshoe Density Plot")


ggplot(ind_long_hs_plus_dens, aes(x = med, y = variable)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = ind_long_hs_plus_dens$low_025, 
                     xmax = ind_long_hs_plus_dens$high_975,
                     height = 0)) +
  theme_tufte() +
  xlim(c(-.5, .5)) +
  geom_vline(xintercept = c(-.5, -.25, 0, .25, .5), colour="grey") +
  ggtitle("Independant Horseshoe Plus Density Plot")

#### out of sample model fit comparison ####

# prepare LOO fit data
ind_fit <- data.frame(no_cl = ind_no_cl  %>% dplyr::select(elpd_loo), 
                         gauss = ind_gauss  %>% dplyr::select(elpd_loo),
                         hs = ind_hs  %>% dplyr::select(elpd_loo),
                         hs_plus = ind_hs_plus %>% dplyr::select(elpd_loo))

# name columns
names(ind_fit) <- c("no_cl", "gauss", "hs", "hs_plus")

# model number for multi-level model
ind_fit$model_num <- 1:100

# make long for use and put values on IC scale(*-2)
ind_fit_long <- ind_fit %>% 
  melt(id.vars = 'model_num') %>% 
  mutate(value = value * -2)

# multi-level model testing difference in fit
ind_basic_loo_model <- lmer(value ~ 1 + (1 | model_num), data = ind_fit_long)
ind_test_loo_model <- lmer(value ~ 1 + variable + (1 | model_num), data = ind_fit_long)

# consistent difference in fit by models
anova(ind_basic_loo_model, ind_test_loo_model)


# extract fixed effect values and SEs for model fit chart

ind_model_fit <- fixef(ind_test_loo_model)
ind_model_fit_se <- se.fixef(ind_test_loo_model)

ind_model_fit_plot <- as.data.frame(cbind(ind_model_fit, ind_model_fit_se))
ind_model_fit_plot$models <- row.names(ind_model_fit_plot)
ind_model_fit_plot <- ind_model_fit_plot %>% filter(models != "(Intercept)")

# plot model fit
ggplot(ind_model_fit_plot, aes(x = models, y = ind_model_fit)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ind_model_fit_plot$ind_model_fit - 
                      1.96*ind_model_fit_plot$ind_model_fit_se,
                    ymax = ind_model_fit_plot$ind_model_fit + 
                      1.96*ind_model_fit_plot$ind_model_fit_se),
                width = .2) +
  coord_flip() +
  theme_tufte() +
  geom_hline(yintercept = c(0, 10, 20, 30), colour="grey") +
  ggtitle("Independant Model LOO Effect Sizes")

#### LOO parameter number analysis ####

# prepare p_LOO fit data
ind_param_num <- data.frame(no_cl = ind_no_cl  %>% dplyr::select(p_loo), 
                               gauss = ind_gauss  %>% dplyr::select(p_loo),
                               hs = ind_hs  %>% dplyr::select(p_loo),
                               hs_plus = ind_hs_plus  %>% dplyr::select(p_loo))

# name columns
names(ind_param_num) <- c("no_cl", "gauss", "hs", "hs_plus")

# model number for multi-level model
ind_param_num$model_num <- 1:100

# make long for use 
ind_param_long <- ind_param_num %>% 
  melt(id.vars = 'model_num')

# multi-level model testing difference in number of parameters
ind_basic_param_model <- lmer(value ~ -1 + (1 | model_num), data = ind_param_long)
ind_test_param_model <- lmer(value ~ -1 + variable + (1 | model_num), data = ind_param_long)

# consistent difference in fit by models
anova(ind_basic_param_model, ind_test_param_model)

# extract fixed effect values and SEs for bar chart

ind_param_numbers <- fixef(ind_test_param_model)
ind_param_numbers_se <- se.fixef(ind_test_param_model)

ind_param_number_plot <- as.data.frame(cbind(ind_param_numbers, ind_param_numbers_se))
ind_param_number_plot$models <- row.names(ind_param_number_plot)

# plot parameter numbers
ggplot(ind_param_number_plot, aes(x = models, y = ind_param_numbers)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ind_param_number_plot$ind_param_numbers - 
                      1.96*ind_param_number_plot$ind_param_numbers_se,
                    ymax = ind_param_number_plot$ind_param_numbers + 
                      1.96*ind_param_number_plot$ind_param_numbers_se),
                width = .2) +
  coord_flip() +
  theme_tufte() +
  geom_hline(yintercept = c(420, 430, 440, 450), colour="grey") +
  ylim(c(420, 455)) +
  ggtitle("Independant Model - Estimated Number of Parameters")








