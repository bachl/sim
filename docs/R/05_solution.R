## ----setup-------------------------------------------------------------------------------------------
pacman::p_load(knitr, tictoc, tidyverse)
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 25,
                                     axis_title_size = 25,
                                     strip_text_size = 20))


## ----Manual misclassification matrix-----------------------------------------------------------------
misclass_prob = matrix(c(
  .80, .10, .15,
  .15, .80, .15,
  .05, .10, .70
), nrow = 3, byrow = TRUE,
dimnames = list(LETTERS[1:3], LETTERS[1:3]))
misclass_prob


## ----Adapted simulation function---------------------------------------------------------------------
sim_misclass = function(pop_dist = c(0.55, 0.3, 0.1, 0.05),
                        n = 1000,
                        misclass_prob = misclass_prob) {
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
  sample_obs = table(sample_true) %>%  
    imap(~ sample(categories,
                  size = .x, replace = TRUE, 
                  prob = misclass_prob[, .y])) %>%
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


## ----Condition---------------------------------------------------------------------------------------
conditions = expand_grid(
  pop_dist = list(c(0.55, 0.3, 0.15)),
  n = 1000,
  misclass_prob = list(misclass_prob)
) %>% 
  rowid_to_column(var = "condition")
conditions


## ----Run simulation----------------------------------------------------------------------------------
i = 1000 # less simulations to save some workshop time 
set.seed(40)
tic()
sims = map_dfr(1:i, ~ conditions) %>%
  rowid_to_column(var = "sim") %>%
  rowwise() %>%
  mutate(res = list(sim_misclass(pop_dist = pop_dist, 
                                 n = n, misclass_prob = misclass_prob)))
toc()


## ----Result plot-------------------------------------------------------------------------------------
sims %>% 
  ungroup() %>% 
  unnest_wider(res) %>% 
  unnest_longer(freq_obs, indices_to = "category") %>% 
  group_by(category) %>%
  summarise(Q = list(quantile(freq_obs, probs = c(0.25, 0.5, 0.75)))) %>%
  unnest_wider(Q) %>%
  ggplot(aes(`50%`, fct_rev(category),
             xmin = `25%`, xmax = `75%`)) +
  geom_pointrange() +
  geom_vline(xintercept = unlist(conditions$pop_dist), color = "red", linetype = 2) + 
  labs(x = str_glue("Median and IQR of the proportions from {i} simulation runs per condition.\nRed lines are shares in the population."),
       y = "Category")


## ----One simulation: draw x--------------------------------------------------------------------------
set.seed(91)
n = 1000
pop_dist_x = 0.5
sample_x_true = rbinom(n = n, 
                         size = 1, 
                         prob = pop_dist_x)
prop.table(table(sample_x_true))


## ----One simulation: draw outcome--------------------------------------------------------------------
pop_baseline = 0.5
pop_difference = 0.1
logit_pop_baseline = log(pop_baseline / (1 - pop_baseline))
pop_oddsratio = ((pop_baseline + pop_difference) / (1 - (pop_baseline + pop_difference))) /
  (pop_baseline / (1 - pop_baseline))
pop_logodds = log(pop_oddsratio)
outcomes_true = rbinom(n = n, size = 1,
                         prob = 1 / (1 + exp(- (logit_pop_baseline + pop_logodds * sample_x_true))))
prop.table(table(outcomes_true, sample_x_true), margin = 2) %>% round(2)


## ----One simulation: misclassification---------------------------------------------------------------
# Misclassification probabilities
accuracy = 0.8
correct_x = rbinom(n = n, size = 1, 
                   prob = (1 - accuracy))
correct_outcome = rbinom(n = n, size = 1, 
                         prob = (1 - accuracy))

# Misclassification
sample_x_obs = abs(sample_x_true - correct_x)
outcomes_obs = abs(outcomes_true - correct_outcome)
mean(sample_x_obs == sample_x_true); mean(outcomes_obs == outcomes_true)


## ----One simulation: quantities----------------------------------------------------------------------
  t_res = t.test(outcomes_obs ~ sample_x_obs)
  (p_obs = t_res$p.value)
  (diff_obs = unname(diff(t_res$estimate)))
  (diff_obs_rel = diff_obs / pop_difference)



## ----New simulation function-------------------------------------------------------------------------
sim_misclass_binary = function(pop_dist_x = 0.5,
                               pop_baseline = 0.5,
                               pop_difference = 0.1,
                               n = 1000,
                               accuracy = 0.8) {
  # Population
  logit_pop_baseline = log(pop_baseline / (1 - pop_baseline))
  pop_oddsratio = ((pop_baseline + pop_difference) / (1 - (pop_baseline + pop_difference))) /
    (pop_baseline / (1 - pop_baseline))
  pop_logodds = log(pop_oddsratio)
  
  # True sample
  sample_x_true = rbinom(n = n, 
                         size = 1, 
                         prob = pop_dist_x)
  outcomes_true = rbinom(n = n, size = 1,
                         prob = 1 / (1 + exp(- (logit_pop_baseline + pop_logodds * sample_x_true))))
  
  # Misclassification probabilities
  correct_x = rbinom(n = n, size = 1, prob = (1 - accuracy))
  correct_outcome = rbinom(n = n, size = 1, prob = (1 - accuracy))
  
  # Misclassification
  sample_x_obs = abs(sample_x_true - correct_x)
  outcomes_obs = abs(outcomes_true - correct_outcome)

  # Model
  t_res = t.test(outcomes_obs ~ sample_x_obs)
  p_obs = t_res$p.value
  diff_obs = unname(diff(t_res$estimate))
  diff_obs_rel = diff_obs / pop_difference
  
  
  # Output
  out = lst(p_obs, diff_obs, diff_obs_rel)
  return(out)
}



## ----Conditions--------------------------------------------------------------------------------------
conditions = expand_grid(
  pop_dist_x = c(.1, .3, .5),
  pop_baseline = c(.1, .3, .5),
  pop_difference = 0.15,
  n = 1000,
  accuracy = seq(from = 0.6, to = 1, by = 0.1)
) %>% 
  rowid_to_column(var = "condition")
conditions


## ----Run simulation 2--------------------------------------------------------------------------------
i = 1000 # less simulations to save some workshop time 
set.seed(40)
tic()
sims = map_dfr(1:i, ~ conditions) %>%
  rowid_to_column(var = "sim") %>%
  rowwise() %>%
  mutate(res = list(sim_misclass_binary(pop_dist_x = pop_dist_x, 
                                        pop_baseline = pop_baseline,
                                        pop_difference = pop_difference,
                                        n = n, accuracy = accuracy)))
toc()


## ----Result plot 2-----------------------------------------------------------------------------------
sims %>% 
  ungroup() %>% 
  unnest_wider(res) %>% 
  group_by(accuracy, pop_baseline, pop_dist_x) %>% 
  summarise(Q = list(quantile(diff_obs_rel, probs = c(0.25, 0.5, 0.75)))) %>% 
  unnest_wider(Q) %>% 
  ggplot(aes(`50%`, factor(accuracy), color = factor(pop_dist_x),
             xmin = `25%`, xmax = `75%`)) +
  geom_pointrange(position = position_dodge(width = -.2)) +
  facet_wrap(vars(pop_baseline), labeller = "label_both") + 
  labs(y = "accuracy",
       x = str_glue("Median and IQR of the ratio true by observed difference\nfrom {i} simulation runs per condition."),
       color = "Population\ntreatment\nprevalence")


## ----Result plot 3-----------------------------------------------------------------------------------
sims %>% 
  ungroup() %>% 
  unnest_wider(res) %>% 
  group_by(accuracy, pop_baseline, pop_dist_x) %>% 
  summarise(P_p05 = mean(p_obs < 0.05)) %>% 
  ggplot(aes(accuracy, P_p05, color = factor(pop_dist_x))) + 
  geom_point() + geom_line() +
  facet_wrap(vars(pop_baseline), labeller = "label_both") + 
  labs(y = "Power at p < .05", color = "Population\ntreatment\nprevalence")

