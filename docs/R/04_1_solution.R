## ----setup-----------------------------------------------------------------------------------------
pacman::p_load(knitr, tictoc, tidyverse)
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 25,
                                     axis_title_size = 25,
                                     strip_text_size = 20))


## ----Simulation function---------------------------------------------------------------------------
# No standard deviation ratio any more because SD fixed at sqrt(lambda)
# M_diff not necessary at this stage, but useful later
sim_ttest_glm = function(n = 200, GR = 1, lambda1 = 1, M_diff = 0) {
  n1 = round(n / (GR + 1))
  n2 = round(n1 * GR)
  lambda2 = lambda1 + M_diff
  g1 = rpois(n = n1, lambda = lambda1)
  g2 = rpois(n = n2, lambda = lambda2)
  Welch = t.test(g1, g2)$p.value
  Student = t.test(g1, g2, var.equal = TRUE)$p.value
  GLM = glm(outcome ~ group,
            data = data.frame(outcome = c(g1, g2),
                              group = c(rep("g1", n1), rep("g2", n2))),
            family = poisson)
  res = lst(Welch, Student, GLM = coef(summary(GLM))[2, 4])
  return(res)
}
sim_ttest_glm()


## ----Conditions------------------------------------------------------------------------------------
# It makes sense to vary group size ratio and lambda
conditions = expand_grid(GR = c(0.5, 1, 2), 
                         n = 600,
                         lambda1 = c(1, 10, 20),
                         M_diff = 0) %>% 
  rowid_to_column(var = "condition")
conditions


## ----Run simulation--------------------------------------------------------------------------------
# use smaller i for now because additional glm() needs time
set.seed(35)
i = 1000
tic()
sims = map_dfr(1:i, ~ conditions) %>%
  rowid_to_column(var = "sim") %>%
  rowwise() %>%
  mutate(p.value = list(sim_ttest_glm(n = n, M_diff = M_diff,
                                 GR = GR, lambda1 = lambda1))) %>% 
  unnest_longer(p.value, indices_to = "method")
toc()


## ----Histogram of p-values-------------------------------------------------------------------------
sims %>% 
  ggplot(aes(p.value)) +
  geom_histogram(binwidth = 0.1, boundary = 0) + 
  facet_grid(method ~ lambda1 + GR, labeller = label_both) + 
  scale_x_continuous(breaks = c(1, 3)/4)


## ----QQ-Plot of p-values---------------------------------------------------------------------------
sims %>% 
  ggplot(aes(sample = p.value, color = method)) +
  geom_qq(distribution = stats::qunif, geom = "line") + 
  geom_abline(slope = 1) + 
  facet_grid(GR ~ lambda1, labeller = label_both)


## ----Rates of p < .05------------------------------------------------------------------------------
sims %>% 
  group_by(GR, lambda1, method) %>% 
  summarise(P_p05 = mean(p.value < 0.05)) %>% 
  ggplot(aes(factor(GR), P_p05, color = method, group = method)) + 
  geom_point() + geom_line() + 
  geom_hline(yintercept = 0.05, linetype = 2) + 
  facet_wrap(vars(lambda1), labeller = label_both) +
  labs(x = "Group size ratio", y = "Proportion p < 0.05")

