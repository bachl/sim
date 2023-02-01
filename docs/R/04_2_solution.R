## ----setup-------------------------------------------------------------------------------------------
pacman::p_load(knitr, tictoc, tidyverse)
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 25,
                                     axis_title_size = 25,
                                     strip_text_size = 20))


## ----Simulation function-----------------------------------------------------------------------------
# We use almost the same simulation function, 
# but we omit Student's t-test, because we already showed its
# problems with unequal group sizes and group SDs
sim_ttest_glm = function(n = 200, GR = 1, lambda1 = 1, M_diff = 0) {
  n1 = round(n / (GR + 1))
  n2 = round(n1 * GR)
  lambda2 = lambda1 + M_diff
  g1 = rpois(n = n1, lambda = lambda1)
  g2 = rpois(n = n2, lambda = lambda2)
  Welch = t.test(g1, g2)$p.value
  GLM = glm(outcome ~ group,
            data = data.frame(outcome = c(g1, g2),
                              group = c(rep("g1", n1), rep("g2", n2))),
            family = poisson)
  res = lst(Welch, GLM = coef(summary(GLM))[2, 4])
  return(res)
}
sim_ttest_glm(n = 100, M_diff = 0.3)


## ----Conditions I------------------------------------------------------------------------------------
# It makes sense to vary group size ratio, lambda, and M_diff
# The positive relationship between n and power is trivial
conditions = expand_grid(GR = c(0.5, 1, 2), 
                         n = 200,
                         lambda1 = c(1, 10, 20),
                         M_diff = c(0.3, 0.5, 0.8)) %>% 
  rowid_to_column(var = "condition")
conditions


## ----Run simulation----------------------------------------------------------------------------------
# use smaller i for now because additional glm() needs time
set.seed(89)
i = 1000
tic()
sims = map_dfr(1:i, ~ conditions) %>%
  rowid_to_column(var = "sim") %>%
  rowwise() %>%
  mutate(p.value = list(sim_ttest_glm(n = n, M_diff = M_diff,
                                      GR = GR, lambda1 = lambda1))) %>% 
  unnest_longer(p.value, indices_to = "method")
toc()


## ----Power plot I------------------------------------------------------------------------------------
sims %>% 
  group_by(GR, lambda1, M_diff, method) %>% 
  summarise(P_p05 = mean(p.value < 0.05)) %>% 
  ggplot(aes(factor(GR), P_p05, color = method, group = method)) + 
  geom_point() + geom_line() +
  facet_grid(M_diff ~ lambda1, labeller = label_both) +
  labs(x = "Group size ratio", y = "Power")


## ----Conditions II-----------------------------------------------------------------------------------
conditions = expand_grid(GR = c(0.5, 1, 2), 
                         n = 200,
                         lambda1 = c(1, 10, 20),
                         d = c(0.2, 0.4, 0.6)) %>% # standardized mean difference.
  mutate(M_diff = d * sqrt(lambda1)) %>% # absolute mean difference
  rowid_to_column(var = "condition")
conditions


## ----Run simulation II-------------------------------------------------------------------------------
# use smaller i for now because additional glm() needs time
set.seed(525)
i = 1000
tic()
sims = map_dfr(1:i, ~ conditions) %>%
  rowid_to_column(var = "sim") %>%
  rowwise() %>%
  mutate(p.value = list(sim_ttest_glm(n = n, M_diff = M_diff,
                                      GR = GR, lambda1 = lambda1))) %>% 
  unnest_longer(p.value, indices_to = "method")
toc()


## ----Power plot II-----------------------------------------------------------------------------------
sims %>% 
  group_by(GR, lambda1, d, method) %>% 
  summarise(P_p05 = mean(p.value < 0.05)) %>% 
  ggplot(aes(factor(GR), P_p05, color = method, group = method)) + 
  geom_point() + geom_line() +
  facet_grid(d ~ lambda1, labeller = label_both, scales = "free_y") +
  labs(x = "Group size ratio", y = "Power")

