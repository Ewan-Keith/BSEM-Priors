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

ggplot(data.frame(x=c(-3,3)), aes(x)) +
  stat_function(fun=gaus, geom="line", aes(colour="Gaussian")) +
  stat_function(fun=laplace, geom="line", aes(colour="Laplace")) +
  stat_function(fun=hs_approx, geom="line", aes(colour="Horseshoe")) +
  stat_function(fun=hsplus_approx, geom="line", aes(colour="Horseshoe+")) +
  ylim(0, 0.6)

ggplot(data.frame(x=c(5,10)), aes(x)) +
  stat_function(fun=gaus, geom="line", aes(colour="Gaussian")) +
  stat_function(fun=laplace, geom="line", aes(colour="Laplace")) +
  stat_function(fun=hs_approx, geom="line", aes(colour="Horseshoe")) +
  stat_function(fun=hsplus_approx, geom="line", aes(colour="Horseshoe+"))


