## code for producing ridge and LASSO trace plots for paper

# load libraries and dataset
library(glmnet)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(gridExtra)
library(broom)
data(QuickStartExample)

#### LASSO Estimate ####
lasso_fit <- glmnet(x, y)
tidied_lasso <- tidy(lasso_fit) %>% filter(term != "(Intercept)")

lasso_gg <- ggplot(tidied_lasso, aes(x = lambda, y = estimate, group = term)) + 
  geom_path(alpha = .4) +
  theme_hc() +
  ggtitle("LASSO Trace") +
  scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1, 1.5), limits = c(-1.2, 1.5))
  

#### ridge regression example ####
ridge_fit <- glmnet(x, y, alpha = 0)
tidied_ridge <- tidy(ridge_fit) %>% filter(term != "(Intercept)")

ridge_gg <- ggplot(tidied_ridge, aes(x = lambda, y = estimate, group = term)) + 
  geom_path(alpha = .4) +
  theme_hc() +
  xlim(0,30) +
  ggtitle("Ridge Regression Trace") +
  scale_y_continuous(breaks = c(-1, -.5, 0, .5, 1, 1.5), limits = c(-1.2, 1.5))

#### join in grid ####

grid.arrange(ridge_gg, lasso_gg, ncol = 2)


