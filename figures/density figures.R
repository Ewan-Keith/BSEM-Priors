library(ggplot2)
library(ggthemes)
library(gridExtra)

gaus <- function(x) (exp(-((x^2)/2))/sqrt(2*pi))

laplace <- function(x) 0.5 * exp(-(abs(x)))

ggplot(data.frame(x=c(-3,3)), aes(x)) +
  stat_function(fun=gaus, geom="line", aes(colour="Gaussian")) +
  stat_function(fun=laplace, geom="line", aes(colour="Laplace"))

k <- 1/sqrt(2*(pi^3))

hs_lower <- function(x) (k/2) * log(1 + 4/(x^2))
hs_upper <- function(x) k * log(1 + 2/(x^2))

ggplot(data.frame(x=c(-3,3)), aes(x)) +
  stat_function(fun=hs_lower, geom="line", aes(colour="lower")) +
  stat_function(fun=hs_upper, geom="line", aes(colour="upper")) 

hs_approx <- function(x) k * log(1 + 2/(x^2))
hsplus_approx <- function(x) 1/(pi^2 * abs(x))

zero_plot <- ggplot(data.frame(x=c(-3,3)), aes(x)) +
  stat_function(fun=gaus, geom="line", aes(colour="Gaussian"), size=1.5) +
  stat_function(fun=laplace, geom="line", aes(colour="Laplace"), size=1.5) +
  stat_function(fun=hs_approx, geom="line", aes(colour="Horseshoe"), size=1.5) +
  stat_function(fun=hsplus_approx, geom="line", aes(colour="Horseshoe+"), size=1.5) +
  ylim(0, 0.6) +
  theme_hc() +
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"))


tail_plot <- ggplot(data.frame(x=c(5,10)), aes(x)) +
  stat_function(fun=gaus, geom="line", aes(colour="Gaussian"), size=1.5) +
  stat_function(fun=laplace, geom="line", aes(colour="Laplace"), size=1.5) +
  stat_function(fun=hs_approx, geom="line", aes(colour="Horseshoe"), size=1.5) +
  stat_function(fun=hsplus_approx, geom="line", aes(colour="Horseshoe+"), size=1.5) +
  theme_hc() +
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"))

#### join in grid ####

grid.arrange(zero_plot, tail_plot, ncol = 2, widths = c(.6, .4))

#### seperate plots ####

gauss_laplace <- ggplot(data.frame(x=c(-3,3)), aes(x)) +
  stat_function(fun=gaus, geom="line", aes(colour="Gaussian"), size=1.5) +
  stat_function(fun=laplace, geom="line", aes(colour="Laplace"), size=1.5) +
  theme_hc() +
  scale_color_grey() +
  ylim(0, 0.6)

laplace_hs <- ggplot(data.frame(x=c(-3,3)), aes(x)) +
  stat_function(fun=hs_approx, geom="line", aes(colour="Horseshoe"), size=1.5) +
  stat_function(fun=laplace, geom="line", aes(colour="Laplace"), size=1.5) +
  theme_hc() +
  scale_color_grey() +
  ylim(0, 0.6)

hs_hsp <- ggplot(data.frame(x=c(-3,3)), aes(x)) +
  stat_function(fun=hsplus_approx, geom="line", aes(colour="Horseshoe+"), size=1.5) +
  stat_function(fun=hs_approx, geom="line", aes(colour="Horseshoe"), size=1.5) +
  theme_hc() +
  scale_color_grey() +
  ylim(0, 0.6)

tail_plot <- ggplot(data.frame(x=c(5,10)), aes(x)) +
  stat_function(fun=gaus, geom="line", aes(colour="Gaussian"), size=1.5) +
  stat_function(fun=laplace, geom="line", aes(colour="Laplace"), size=1.5) +
  stat_function(fun=hs_approx, geom="line", aes(colour="Horseshoe"), size=1.5) +
  stat_function(fun=hsplus_approx, geom="line", aes(colour="Horseshoe+"), size=1.5) +
  theme_hc()  +
  scale_color_grey()

grid.arrange(gauss_laplace, laplace_hs, hs_hsp, tail_plot, ncol = 2, nrow=2)
