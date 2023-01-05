## ----setup------------------------------------------------------------------------------------------------------------
pacman::p_load(knitr, tictoc, extraDistr, tidyverse)
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 25,
                                     axis_title_size = 25,
                                     strip_text_size = 20))


## ---------------------------------------------------------------------------------------------------------------------
set.seed(28)
pop_dist = c(0.55, 0.3, 0.1, 0.05)
k = length(pop_dist) # number of categories
categories = LETTERS[1:k] # names of the categories
names(pop_dist) = categories
pop_dist


## ---------------------------------------------------------------------------------------------------------------------
n = 1000 # sample size
sample_true = sample(x = categories, 
                     size = n, 
                     replace = TRUE, 
                     prob = pop_dist) %>% 
  factor(levels = categories) %>% # if any categories not observed in the sample
  sort() # important later for efficient misclassification
freq_true = sample_true %>% 
  table() %>% 
  prop.table()
freq_true


## ---------------------------------------------------------------------------------------------------------------------
# Simple: Equal error rates, equal difficulties, one process
accuracy = 0.8
equal_error = (1 - accuracy) / (k - 1)
misclass_prop = matrix(equal_error, nrow = k, ncol = k, 
                       dimnames = list(categories, categories))
diag(misclass_prop) = accuracy
misclass_prop


## ---------------------------------------------------------------------------------------------------------------------
tic()
sample_obs = sample_true %>%  # n samples
  map_chr(~ sample(categories,
                  size = 1, replace = FALSE, 
                  prob = misclass_prop[, .x])) %>%
    factor(levels = categories)
freq_obs = sample_obs %>% 
    table() %>% 
    prop.table()
toc()
freq_obs


## ---------------------------------------------------------------------------------------------------------------------
tic()
sample_obs = table(sample_true) %>%  # k samples
  imap(~ sample(categories,
                  size = .x, replace = TRUE, 
                  prob = misclass_prop[, .y])) %>%
    unlist(use.names = FALSE) %>% 
    factor(levels = categories)
freq_obs = sample_obs %>% 
  table() %>% 
    prop.table()
toc()
freq_obs


## ---------------------------------------------------------------------------------------------------------------------
sqrt(mean((pop_dist - freq_obs)^2)) %>% round(2)


## ---------------------------------------------------------------------------------------------------------------------
sqrt(mean((freq_true - freq_obs)^2)) %>% round(2)


## ---------------------------------------------------------------------------------------------------------------------
sqrt(mean((pop_dist - freq_true)^2)) %>% round(2)


## ---------------------------------------------------------------------------------------------------------------------
sim_misclass = function(pop_dist = c(0.55, 0.3, 0.1, 0.05),
                        n = 1000,
                        accuracy = 0.8) {
  # Population
  k = length(pop_dist)
  categories = LETTERS[1:k]
  
  # Sample
  sample_true = sample(x = categories, 
                       size = n, 
                       replace = TRUE, 
                       prob = pop_dist) %>% 
    factor(levels = categories) %>% 
    sort()
  freq_true = sample_true %>% 
    table() %>% 
    prop.table()
  
  # Misclassification
  equal_error = (1 - accuracy) / (k - 1)
  misclass_prop = matrix(equal_error, nrow = k, ncol = k,
                         dimnames = list(categories, categories))
  diag(misclass_prop) = accuracy

  sample_obs = table(sample_true) %>%  
    imap(~ sample(categories,
                  size = .x, replace = TRUE, 
                  prob = misclass_prop[, .y])) %>%
    unlist(use.names = FALSE) %>% 
    factor(levels = categories)
  freq_obs = sample_obs %>% 
    table() %>% 
    prop.table()

  # Error summary
  rmse = sqrt(mean((pop_dist - freq_obs)^2))

  # Output
  out = lst(freq_true, freq_obs, rmse)
  out
}


## ---------------------------------------------------------------------------------------------------------------------
conditions = expand_grid(
  pop_dist = list(c(0.55, 0.3, 0.1, 0.05)),
  n = 1000,
  accuracy = seq(from = 0.4, to = 1, by = 0.1)
  ) %>% 
  rowid_to_column(var = "condition")
conditions


## ---------------------------------------------------------------------------------------------------------------------
i = 1000 # less simulations to save some workshop time 
set.seed(39)
tic()
sims = map_dfr(1:i, ~ conditions) %>%
  rowid_to_column(var = "sim") %>%
  rowwise() %>%
  mutate(res = list(sim_misclass(pop_dist = pop_dist, 
                                 n = n, accuracy = accuracy)))
toc()


## ---------------------------------------------------------------------------------------------------------------------
sims %>% 
  ungroup() %>% 
  unnest_wider(res) %>% 
  unnest_longer(freq_obs, indices_to = "category") %>% 
  group_by(accuracy, category) %>% 
  summarise(Q = list(quantile(freq_obs, probs = c(0.25, 0.5, 0.75)))) %>% 
  unnest_wider(Q) %>% 
  ggplot(aes(`50%`, factor(accuracy), 
             xmin = `25%`, xmax = `75%`, color = category)) +
  geom_pointrange() +
  labs(x = str_glue("Median and IQR of the proportions from {i} simulation runs per condition."),
       y = "accuracy")


## ---------------------------------------------------------------------------------------------------------------------
sims %>% 
  ungroup() %>% 
  unnest_wider(res) %>% 
  group_by(accuracy) %>% 
  summarise(Q = list(quantile(rmse, probs = c(0.25, 0.5, 0.75)))) %>% 
  unnest_wider(Q) %>% 
  ggplot(aes(accuracy, `50%`, ymin = `25%`, ymax = `75%`)) + 
  geom_pointrange() + 
  geom_line() +
  labs(y = "RMSE (Mdn & IQR)",
       x = "accuracy")


## ---------------------------------------------------------------------------------------------------------------------
## list(pd1 = c(0.25, 0.25, 0.25, 0.25),
##      pd2 = c(0.55, 0.30, 0.10, 0.05))


## ---------------------------------------------------------------------------------------------------------------------
rdirichlet(n = 3, alpha = c(1,1,1,1)) %>% round(2)


## ---------------------------------------------------------------------------------------------------------------------
i = 5000
set.seed(39)
conditions = expand_grid(
  n = 1000,
  accuracy = seq(from = 0.4, to = 1, by = 0.2)
  ) %>% 
  rowid_to_column(var = "condition")
conditions = map_dfr(1:i, ~ conditions) %>%
  rowid_to_column(var = "sim") %>%
  mutate(pop_dist = split(rdirichlet(nrow(.), c(1,1,1,1)), 1:nrow(.)))
conditions


## ---------------------------------------------------------------------------------------------------------------------
tic()
sims = conditions %>% 
  rowwise() %>%
  mutate(res = list(sim_misclass(pop_dist = pop_dist, n = n, accuracy = accuracy)))
toc()


## ---------------------------------------------------------------------------------------------------------------------
sims %>% 
  ungroup() %>% 
  unnest_wider(res) %>% 
  mutate(chi2_pop_dist = map_dbl(pop_dist, ~ chisq.test(.x*100)$statistic)) %>% 
  ggplot(aes(chi2_pop_dist)) +
  geom_histogram()


## ---------------------------------------------------------------------------------------------------------------------
sims %>% 
  ungroup() %>% 
  unnest_wider(res) %>% 
  mutate(chi2_pop_dist = map_dbl(pop_dist, ~ chisq.test(.x*100)$statistic)) %>% 
  ggplot(aes(chi2_pop_dist, rmse, color = factor(accuracy))) +
  geom_smooth() +
  xlim(0, 155) +
  labs(x = "chi2_pop_dist",
       color = "accuracy",
       y = "RMSE",
       caption = "method = 'gam' and formula 'y ~ s(x, bs = 'cs')")


## ---------------------------------------------------------------------------------------------------------------------
sims %>% 
  ungroup() %>% 
  unnest_wider(res) %>% 
  mutate(chi2_pop_dist = map_dbl(pop_dist, ~ chisq.test(.x*100)$statistic)) %>% 
  filter(round(chi2_pop_dist) %in% c(0, 25, 50, 75, 100, 125, 150)) %>% 
  group_by(round(chi2_pop_dist)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(chi2_pop_dist) %>% 
  select(chi2_pop_dist, pop_dist) %>% 
  mutate(pop_dist = map(pop_dist, sort, decreasing = TRUE)) %>% 
  unnest_wider(pop_dist) %>% 
  kable(digits = 2, col.names = c("chi2_pop_dist", LETTERS[1:4]))


## ---------------------------------------------------------------------------------------------------------------------
set.seed(4026)
pop_dist = c(0.55, 0.3, 0.1, 0.05)
k = length(pop_dist) # number of categories
categories = LETTERS[1:k] # names of the categories
names(pop_dist) = categories
pop_dist


## ---------------------------------------------------------------------------------------------------------------------
n = 1000 # sample size
sample_true = sample(x = categories, 
                     size = n, 
                     replace = TRUE, 
                     prob = pop_dist) %>% 
  factor(levels = categories) %>% # if any categories not observed in the sample
  sort() # important later for efficient misclassification
freq_true = sample_true %>% 
  table() %>% 
  prop.table()
freq_true


## ---------------------------------------------------------------------------------------------------------------------
lambdas = c(21:24)
names(lambdas) = categories
outcomes = table(sample_true) %>% 
  imap(~ rpois(n = .x, lambda = lambdas[.y])) %>% 
  unlist(use.names = FALSE)
table(sample_true, outcomes)


## ---------------------------------------------------------------------------------------------------------------------
# Simple: Equal error rates, equal difficulties, one process
accuracy = 0.8
equal_error = (1 - accuracy) / (k - 1)
misclass_prop = matrix(equal_error, nrow = k, ncol = k, 
                       dimnames = list(categories, categories))
diag(misclass_prop) = accuracy
misclass_prop


## ---------------------------------------------------------------------------------------------------------------------
tic()
sample_obs = table(sample_true) %>%  # k samples
  imap(~ sample(categories,
                  size = .x, replace = TRUE, 
                  prob = misclass_prop[, .y])) %>%
    unlist(use.names = FALSE) %>% 
    factor(levels = categories)
freq_obs = sample_obs %>% 
  table() %>% 
    prop.table()
toc()
freq_obs


## ---------------------------------------------------------------------------------------------------------------------
t_res = categories[2:k] %>% 
  map(~ t.test(outcomes[sample_true == "A"],
               outcomes[sample_true == .x])) %>% 
  set_names(str_c("A_", categories[2:k]))


## ---------------------------------------------------------------------------------------------------------------------
t_res %>% 
  map_dbl(~ diff(.x$estimate)) %>% round(2)


## ---------------------------------------------------------------------------------------------------------------------
t_res %>% 
  map_dbl(~ .x$p.value) %>% round(5)


## ---------------------------------------------------------------------------------------------------------------------
sim_misclass_test = function(pop_dist = c(0.55, 0.3, 0.1, 0.05),
                        lambdas = c(3:6),
                        n = 1000,
                        accuracy = 0.8) {
  # Population
  k = length(pop_dist)
  categories = LETTERS[1:k]
  names(lambdas) = categories
  
  # True sample
  sample_true = sample(x = categories, 
                       size = n, 
                       replace = TRUE, 
                       prob = pop_dist) %>% 
    factor(levels = categories) %>% 
    sort()
  freq_true = sample_true %>% 
    table() %>% 
    prop.table()
  outcomes = table(sample_true) %>% 
    imap(~ rpois(n = .x, lambda = lambdas[.y])) %>% 
    unlist(use.names = FALSE)

  # Misclassification probabilities
  equal_error = (1 - accuracy) / (k - 1)
  misclass_prop = matrix(equal_error, nrow = k, ncol = k, 
                         dimnames = list(categories, categories))
  diag(misclass_prop) = accuracy
  
  # Misclassification  
  sample_obs = table(sample_true) %>%  
    imap(~ sample(categories,
                  size = .x, replace = TRUE, 
                  prob = misclass_prop[, .y])) %>%
    unlist(use.names = FALSE) %>% 
    factor(levels = categories)
  freq_obs = sample_obs %>% 
    table() %>% 
    prop.table()
  
  # Model
  ttests_obs = categories[2:k] %>% 
    map(~ t.test(outcomes[sample_obs == "A"],
                 outcomes[sample_obs == .x])) %>% 
    set_names(str_c("A_", categories[2:k]))

  # Error summary for categories
  rmse = sqrt(mean((pop_dist - freq_obs)^2))

  # Output
  out = lst(freq_true, freq_obs, rmse,
            p_obs = ttests_obs %>% 
              map_dbl(~ .x$p.value),
            diff_obs = ttests_obs %>% 
              map_dbl(~ diff(.x$estimate))
            )
  out
}


## ---------------------------------------------------------------------------------------------------------------------
conditions = expand_grid(
  pop_dist = list(c(0.55, 0.3, 0.1, 0.05)),
  lambdas = list(start_1 = c(1:4),
                 start_21 = c(21:24)),
  n = 1000,
  accuracy = seq(from = 0.4, to = 1, by = 0.2)
) %>% 
  rowid_to_column(var = "condition")
conditions


## ---------------------------------------------------------------------------------------------------------------------
i = 1000
set.seed(39)
tic()
sims = map_dfr(1:i, ~ conditions) %>%
  rowid_to_column(var = "sim") %>%
  rowwise() %>%
  mutate(res = list(sim_misclass_test(
    pop_dist = pop_dist,
    lambdas = lambdas, 
    n = n, accuracy = accuracy)))
toc()


## ---------------------------------------------------------------------------------------------------------------------
sims %>% 
  ungroup() %>% 
  unnest_wider(res) %>% 
  unnest_longer(diff_obs, indices_to = "difference") %>% 
  mutate(lambdas = names(lambdas)) %>% 
  group_by(accuracy, difference, lambdas) %>% 
  summarise(Q = list(quantile(diff_obs, probs = c(0.25, 0.5, 0.75)))) %>% 
  unnest_wider(Q) %>% 
  ggplot(aes(`50%`, factor(accuracy),
             xmin = `25%`, xmax = `75%`, color = difference)) +
  geom_pointrange() +
  facet_wrap(vars(lambdas)) + 
  labs(y = "accuracy",
       x = str_glue("Median and IQR of the mean differences from {i} simulation runs per condition."))


## ---------------------------------------------------------------------------------------------------------------------
sims %>% 
  ungroup() %>% 
  unnest_wider(res) %>% 
  unnest_longer(diff_obs, indices_to = "difference") %>% 
  mutate(
    diff_true = case_when(
      difference == "A_B" ~ 1L,
      difference == "A_C" ~ 2L,
      difference == "A_D" ~ 3L
    ),
    recovered = diff_obs / diff_true,
    lambdas = names(lambdas)
    ) %>% 
  select(accuracy, difference, lambdas, diff_obs, diff_true, recovered) %>% 
  group_by(accuracy, difference, lambdas) %>% 
  summarise(Q = list(quantile(recovered, probs = c(0.25, 0.5, 0.75)))) %>% 
  unnest_wider(Q) %>% 
  ggplot(aes(`50%`, factor(accuracy),
             xmin = `25%`, xmax = `75%`, color = difference)) +
  geom_pointrange() +
  facet_wrap(vars(lambdas)) + 
  labs(y = "accuracy",
       x = str_glue("Median and IQR of the ratio true by observed difference\nfrom {i} simulation runs per condition."))


## ---------------------------------------------------------------------------------------------------------------------
sims %>% 
  ungroup() %>% 
  mutate(lambdas = names(lambdas)) %>% 
  unnest_wider(res) %>% 
  unnest_longer(p_obs, indices_to = "difference") %>% 
  group_by(accuracy, difference, lambdas) %>% 
  summarise(P_p05 = mean(p_obs < 0.05)) %>% 
  ggplot(aes(accuracy, P_p05, color = difference)) + 
  geom_point() + geom_line() +
  facet_wrap(vars(lambdas)) + 
  labs(y = "Power at p < .05")

