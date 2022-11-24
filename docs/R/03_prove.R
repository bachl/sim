## ----setup, include=FALSE-------------------------------------------------------------------------------
pacman::p_load(knitr, tidyverse)
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 25, axis_title_size = 25))


## -------------------------------------------------------------------------------------------------------
dnorm(x = 0, mean = 0, sd = 1, log = FALSE)
dnorm(x = seq(-2.5, 2.5, by = 0.5), mean = 0, sd = 1, log = FALSE)


## -------------------------------------------------------------------------------------------------------
tibble(x = seq(-2.5, 2.5, by = 0.5), y = dnorm(x)) %>% 
  ggplot(aes(x, y)) + geom_point() + stat_function(fun = dnorm)


## -------------------------------------------------------------------------------------------------------
pnorm(q = 0, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


## -------------------------------------------------------------------------------------------------------
2 * (1 - pnorm(q = 1.96))


## -------------------------------------------------------------------------------------------------------
qnorm(p = 0.5, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


## -------------------------------------------------------------------------------------------------------
qnorm(p = c(0.025, 0.975), mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


## -------------------------------------------------------------------------------------------------------
rnorm(n = 10, mean = 0, sd = 1)
rnorm(n = 10, mean = 0, sd = 1)


## -------------------------------------------------------------------------------------------------------
set.seed(12)
(sim1 = rnorm(n = 10, mean = 0, sd = 1))
set.seed(12)
(sim2 = rnorm(n = 10, mean = 0, sd = 1))
sim1 == sim2


## -------------------------------------------------------------------------------------------------------
LETTERS[1:6]


## -------------------------------------------------------------------------------------------------------
sample(x = LETTERS[1:6], size = 3, replace = FALSE, prob = NULL)


## -------------------------------------------------------------------------------------------------------
sample(x = LETTERS[1:6], size = 3, replace = TRUE, prob = 1:6)


## -------------------------------------------------------------------------------------------------------
sample(x = LETTERS[1:6])


## -------------------------------------------------------------------------------------------------------
sample(x = LETTERS[1:6], replace = TRUE)


## -------------------------------------------------------------------------------------------------------
set.seed(436) # make results reproducible 
mu = 5
sigma = 3
ggplot() + 
  stat_function(fun = dnorm,
                args = list(mean = mu, sd = sigma),
                xlim = c(mu-3*sigma, mu+3*sigma))


## -------------------------------------------------------------------------------------------------------
n = 100
(one_sim = rnorm(n = n, mean = mu, sd = sigma)) %>% round(2)


## -------------------------------------------------------------------------------------------------------
mean(one_sim)


## -------------------------------------------------------------------------------------------------------
sim_CLT_normal = function(n, mu, sigma) {
  sims = rnorm(n = n, mean = mu, sd = sigma) # Draw sample
  mean(sims) # Compute mean
}
sim_CLT_normal(n = n, mu = mu, sigma = sigma)


## -------------------------------------------------------------------------------------------------------
i = 10000 # number of simulations
many_means = map_dbl(1:i, .f = ~ sim_CLT_normal(n = n, mu = mu, sigma = sigma))
many_means[1:20] %>% round(2)


## -------------------------------------------------------------------------------------------------------
mu; mean(many_means)
sigma / sqrt(n); sd(many_means)


## -------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------
set.seed(89) # make results reproducible 
lower = 2 # lower bound of the uniform distribution
upper = 12 # upper bound of the uniform distribution
n = 100
mu = (lower + upper) / 2 # mean of a uniform distribution
sigma = sqrt(1/12 * (upper - lower)^2) # standard deviation of a uniform distribution
ggplot() + 
  stat_function(fun = dunif,
                args = list(min = lower, max = upper),
                xlim = c(mu-3*sigma, mu+3*sigma)) + 
  geom_vline(xintercept = mu) + labs(x = NULL)


## -------------------------------------------------------------------------------------------------------
# new function
sim_CLT_uniform = function(n, lower, upper) {
  sims = runif(n = n, min = lower, max = upper)
  mean(sims)
}
i = 10000
many_means = map_dbl(1:i, .f = ~ sim_CLT_uniform(n = n, lower = lower, upper = upper))


## -------------------------------------------------------------------------------------------------------
mu; mean(many_means)
sigma / sqrt(n); sd(many_means)


## -------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------
set.seed(357) # make results reproducible 
prob_one = 0.4 # Probability of a success (1, not 0)
n = 100
mu = prob_one # Because of CLT ;)
sigma = sqrt(prob_one * (1 - prob_one)) # Standard deviation of a binomial distribution
ggplot() + 
  stat_function(fun = dbinom,
                args = list(size = 1, prob = prob_one),
                xlim = c(0, 1),
                n = 2,
                geom = "bar") + 
  geom_vline(xintercept = mu) + labs(x = NULL)


## -------------------------------------------------------------------------------------------------------
# new function
sim_CLT_binom = function(n, prob_one) {
  sims = rbinom(n = n, size = 1, prob = prob_one)
  mean(sims) # mean = prop(1)
}
i = 10000
many_means = map_dbl(1:i, .f = ~ sim_CLT_binom(n = n, prob_one = prob_one))


## -------------------------------------------------------------------------------------------------------
mu; mean(many_means)
sigma / sqrt(n); sd(many_means)


## -------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------
# new function
sim_CLT_pois = function(n, lambda) {
  sims = rpois(n = n, lambda = lambda)
  mean(sims)
}
i = 10000
many_means = map_dbl(1:i, .f = ~ sim_CLT_pois(n = n, lambda = lambda))


## -------------------------------------------------------------------------------------------------------
mu; mean(many_means)
sigma / sqrt(n); sd(many_means)


## -------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------
# new function
sim_CLT_t = function(n, df) {
  sims = rt(n = n, df = df)
  mean(sims)
}
i = 10000
many_means = map_dbl(1:i, .f = ~ sim_CLT_t(n = n, df = df))


## -------------------------------------------------------------------------------------------------------
mu; mean(many_means)
sigma / sqrt(n); sd(many_means)


## -------------------------------------------------------------------------------------------------------
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

