#### analysis of BSEM simulation results
library(dplyr)
library(reshape2)
library(ggplot2)
library(GGally)
library(ggthemes)

#### sparse case ####

# read data
sparse_gauss <- read.csv("sim_results/sparse_gaus.csv")
sparse_lasso <- read.csv("sim_results/sparse_lasso.csv")
sparse_hs <- read.csv("sim_results/sparse_hs.csv")
sparse_hs_plus <- read.csv("sim_results/sparse_hs_plus.csv")
sparse_no_cl <- read.csv("sim_results/sparse_no_cl.csv")

# prepare cl estimates bias data
sparse_cl_bias <- data.frame(gauss = sparse_gauss  %>% select(RMSE),
                             lasso = sparse_lasso  %>% select(RMSE), 
                             hs = sparse_hs  %>% select(RMSE),
                             hs_plus = sparse_hs_plus  %>% select(RMSE))

# name columns
names(sparse_cl_bias) <- c("gauss", "lasso", "hs", "hs_plus")

# long form for plotting
sparse_cl_bias_long <- sparse_cl_bias %>% melt()

# ggplot box plot
sparse_cl_bias %>% melt() %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot()

## para-coord plot of cl bias
ggparcoord(sparse_cl_bias, alphaLines = .2, scale = 'globalminmax') + 
  stat_summary(fun.y=median, aes(group = 1), 
               colour='black', geom='line',
               size = 1.5) + theme_hc() +
  scale_x_discrete(expand=c(0,0))

# prepare LOO fit data
sparse_fit <- data.frame(no_cl = sparse_no_cl  %>% select(elpd_loo), 
                             gauss = sparse_gauss  %>% select(elpd_loo),
                             lasso = sparse_lasso  %>% select(elpd_loo), 
                             hs = sparse_hs  %>% select(elpd_loo),
                             hs_plus = sparse_hs_plus  %>% select(elpd_loo))

# name columns
names(sparse_fit) <- c("no_cl", "gauss", "lasso", "hs", "hs_plus")

# make long for use
sparse_fit_long <- sparse_fit %>% melt()

# prepare LOO-SE fit data
sparse_fit_se <- data.frame(no_cl = sparse_no_cl  %>% select(elpd_loo_se), 
                         gauss = sparse_gauss  %>% select(elpd_loo_se),
                         lasso = sparse_lasso  %>% select(elpd_loo_se), 
                         hs = sparse_hs  %>% select(elpd_loo_se),
                         hs_plus = sparse_hs_plus  %>% select(elpd_loo_se))

# name columns
names(sparse_fit_se) <- c("no_cl", "gauss", "lasso", "hs", "hs_plus")

# make long for use
sparse_fit_se_long <- sparse_fit_se %>% melt()

# extract median fit
sparse_fit_plot_data <- sparse_fit_long %>% 
  group_by(variable) %>% 
  summarise(med_fit = median(value))

#extract median fit SE
temp_se_data <- sparse_fit_se_long %>% 
  group_by(variable) %>% 
  summarise(med_se = median(value))

# merge the above two datasets for plotting
sparse_fit_se_plot_data <- merge(sparse_fit_plot_data, temp_se_data)

#produe upper and lower estimates for errorbar
sparse_fit_se_plot_data <- sparse_fit_se_plot_data %>% 
  mutate(fit_upper = med_fit + med_se,
         fit_lower = med_fit - med_se)

## plot fit graph
ggplot(sparse_fit_se_plot_data, aes(x = variable, y = med_fit)) +
  geom_point(shape = 18, size = 10) +
  geom_errorbar(aes(ymin = fit_lower, ymax = fit_upper)) +
  theme_hc()





sparse_gauss  %>% melt() %>% filter(variable != 'RMSE',
                                    variable != 'elpd_loo',
                                    variable != 'elpd_loo_se',
                                    variable != 'p_loo',
                                    variable != 'p_loo_se') %>% 
  ggplot(aes(x = value)) + 
  geom_density() +
  facet_wrap('variable')



