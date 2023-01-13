## ----setup---------------------------------------------------------------------------------------------------
pacman::p_load(knitr, tictoc, tidyverse)
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 25,
                                     axis_title_size = 25,
                                     strip_text_size = 20))


## ----Poisson distribution plot-------------------------------------------------------------------------------
# Population distribution: Poisson
set.seed(825) # make results reproducible 
lambda = 5
n = 100
mu = lambda
sigma = sqrt(lambda)
ggplot() + 
  stat_function(fun = dpois,
                n = 16,
                xlim = c(0,15),
                args = list(lambda = lambda),
                geom = "bar") + 
  geom_vline(xintercept = mu) + labs(x = NULL)


## ----Poisson CLT simulation function-------------------------------------------------------------------------
# new function
sim_CLT_pois = function(n, lambda) {
  sims = rpois(n = n, lambda = lambda)
  mean(sims)
}
i = 10000
many_means = map_dbl(1:i, .f = ~ sim_CLT_pois(n = n, lambda = lambda))


## ----Poisson mean and standard error-------------------------------------------------------------------------
mu; mean(many_means)
sigma / sqrt(n); sd(many_means)


## ----Poisson Histogram and Q-Q-Plot--------------------------------------------------------------------------
d_many_means = tibble(sim = 1:i, mu_hat = many_means)
d_many_means %>% 
  ggplot(aes(mu_hat)) + geom_histogram() + 
  geom_vline(xintercept = c(mu - sigma / sqrt(n), mu, mu + sigma / sqrt(n))) +
  geom_vline(xintercept = c(mean(many_means)-sd(many_means),
                            mean(many_means),
                            mean(many_means)+sd(many_means)), 
             color = "red", linetype = 2)

d_many_means %>% 
  ggplot(aes(sample = many_means)) +
  geom_qq(distribution = qnorm, 
          dparams = c(mean = mu, sd = sigma / sqrt(n)), 
          geom = "line") + 
  geom_abline(slope = 1)


## ----Student T distribution plot-----------------------------------------------------------------------------
set.seed(951) # make results reproducible 
df = 5 # degrees of freedom
n = 100
mu = 0 # Central T
sigma = sqrt(df/(df - 2)) # Standard deviation of a T distribution
ggplot() + 
  stat_function(fun = dt,
                args = list(df = df),
                xlim = c(-3,3)) + 
  geom_vline(xintercept = mu) + labs(x = NULL)


## ----Student T CLT simulation function-----------------------------------------------------------------------
# new function
sim_CLT_t = function(n, df) {
  sims = rt(n = n, df = df)
  mean(sims)
}
i = 10000
many_means = map_dbl(1:i, .f = ~ sim_CLT_t(n = n, df = df))


## ----Student T mean and standard error-----------------------------------------------------------------------
mu; mean(many_means)
sigma / sqrt(n); sd(many_means)


## ----Student T histogram and Q-Q-plot------------------------------------------------------------------------
d_many_means = tibble(sim = 1:i, mu_hat = many_means)
d_many_means %>% 
  ggplot(aes(mu_hat)) + geom_histogram() + 
  geom_vline(xintercept = c(mu - sigma / sqrt(n), mu, mu + sigma / sqrt(n))) +
  geom_vline(xintercept = c(mean(many_means)-sd(many_means),
                            mean(many_means),
                            mean(many_means)+sd(many_means)), 
             color = "red", linetype = 2)

d_many_means %>% 
  ggplot(aes(sample = many_means)) +
  geom_qq(distribution = qnorm, 
          dparams = c(mean = mu, sd = sigma / sqrt(n)), 
          geom = "line") + 
  geom_abline(slope = 1)

