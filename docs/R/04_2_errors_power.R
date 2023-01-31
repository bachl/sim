## ----setup--------------------------------------------------------------------------------------------------------------------
pacman::p_load(knitr, mgcv, tictoc, tidyverse)
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 25,
                                     axis_title_size = 25,
                                     strip_text_size = 20))


## ----Simulation function------------------------------------------------------------------------------------------------------
sim_ttest = function(n = 200, GR = 1, SDR = 1, M_diff = 0) {
  n1 = round(n / (GR + 1))
  n2 = round(n1 * GR)
  sd1 = 1
  sd2 = sd1 * SDR # sd2/sd1
  g1 = rnorm(n = n1, mean = 0, sd = sd1) # control
  g2 = rnorm(n = n2, mean = M_diff, sd = sd2) # treatment
  welch = t.test(g1, g2)$p.value
  student = t.test(g1, g2, var.equal = TRUE)$p.value
  res = list("Welch" = welch, "Student" = student)
  res
}
sim_ttest(n = 100, M_diff = 0.3)


## ----Experimental conditions--------------------------------------------------------------------------------------------------
conditions = expand_grid(
  GR = 1, 
  SDR = 1,
  n = c(100, 200, 300),
  M_diff = c(0.2, 0.4, 0.6)) %>% 
  rowid_to_column(var = "condition")
conditions


## ----Run simulation-----------------------------------------------------------------------------------------------------------
set.seed(326)
i = 10000
tic()
sims = map_dfr(1:i, ~ conditions) %>% # each condition i times
  rowid_to_column(var = "sim") %>% # within simulation comparison
  rowwise() %>%
  mutate(p.value = list(sim_ttest(n = n, M_diff = M_diff))) %>% 
  unnest_longer(p.value, indices_to = "method")
toc()


## ----Compare with analytical solution - sim results---------------------------------------------------------------------------
sims %>% 
  filter(M_diff == 0.4 & method == "Student") %>% 
  group_by(n) %>% 
  summarise(P_p05 = mean(p.value < 0.05)) %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), round, 3))


## ----Compare with analytical solution - analytical results--------------------------------------------------------------------
power.t.test(n = c(100, 200, 300)/2,
             delta = 0.4, sd = 1,
             sig.level = 0.05)


## ----Plot Power by method-----------------------------------------------------------------------------------------------------
sims %>% 
  group_by(n, M_diff, method) %>% 
  summarise(P_p05 = mean(p.value < 0.05)) %>% 
  ggplot(aes(factor(n), P_p05, color = method, group = method)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) + 
  geom_line(position = position_dodge(width = 0.3), size = 1.5) +
  facet_wrap(vars(M_diff), labeller = label_both) +
  labs(x = "n", y = "Power")


## ----Simulation function 2----------------------------------------------------------------------------------------------------
sim_ttest = function(n = 200, GR = 1, SDR = 1, M_diff = 0) {
  n1 = round(n / (GR + 1))
  n2 = round(n1 * GR)
  sd1 = 1
  sd2 = sd1 * SDR # sd2/sd1
  g1 = rnorm(n = n1, mean = 0, sd = sd1)
  g2 = rnorm(n = n2, mean = M_diff, sd = sd2)
  res = t.test(g1, g2)$p.value
  return(res)
}
sim_ttest(n = 300, M_diff = 0.4)


## ----Conditions 2a------------------------------------------------------------------------------------------------------------
conditions = expand_grid(GR = 1, 
                         SDR = 1,
                         n = seq(from = 50, to = 500, by = 10),
                         M_diff = 0.4) %>% 
  rowid_to_column(var = "condition")
i = 250
conditions = map_dfr(1:i, ~ conditions) %>%
  rowid_to_column(var = "sim")
conditions


## ----Conditions 2b------------------------------------------------------------------------------------------------------------
i = 250*46 # same total number of simulations
set.seed(86)
conditions = tibble(sim = 1:i,
                    GR = 1, 
                    SDR = 1,
                    n = sample(x = 50:500, size = i, replace = TRUE),
                    M_diff = 0.4)
conditions


## ----Run simulation 2---------------------------------------------------------------------------------------------------------
tic()
sims = conditions %>% 
  rowwise() %>%
  mutate(p.value = sim_ttest(n = n, M_diff = M_diff, GR = GR, SDR = SDR))
toc()


## ----Plot power curve---------------------------------------------------------------------------------------------------------
sims %>% 
  mutate(p05 = as.integer(p.value < 0.05)) %>% 
  ggplot(aes(n, p05)) +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cr"), 
              method.args = list(family = "binomial")) + 
  labs(y = "Power")


## ----Fit GAM------------------------------------------------------------------------------------------------------------------
power_mod = sims %>% 
  mutate(p05 = as.integer(p.value < 0.05)) %>%
  gam(p05 ~ s(n, bs = "cr"), family = binomial, data = .)


## ----Predict power from gam---------------------------------------------------------------------------------------------------
tibble(n = c(120, 256, 404)) %>% 
  mutate(power = predict(power_mod, newdata = .,
                         type = "response"))


## ----Compare with analytical solution-----------------------------------------------------------------------------------------
power.t.test(n = c(120, 256, 404)/2,
             delta = 0.4, sd = 1, 
             sig.level = 0.05)


## ----Simulation function 3----------------------------------------------------------------------------------------------------
sim_ttest = function(n = 200, GR = 1, SDR = 1, M_diff = 0) {
  n1 = round(n / (GR + 1))
  n2 = round(n1 * GR)
  sd1 = 1
  sd2 = sd1 * SDR # sd2/sd1
  g1 = rnorm(n = n1, mean = 0, sd = sd1)
  g2 = rnorm(n = n2, mean = M_diff, sd = sd2)
  res = t.test(g1, g2)$p.value
  return(res)
}
sim_ttest(n = 300, M_diff = 0.3, GR = 0.5)


## ----Conditions 3-------------------------------------------------------------------------------------------------------------
conditions = expand_grid(GR = c(0.5, 1, 2), 
                         SDR = c(0.5, 1, 2),
                         n = 300,
                         M_diff = 0.3) %>% 
  rowid_to_column(var = "condition") %>% 
  mutate(
    n1 = round(n / (GR + 1)),
    n2 = round(n1 * GR),
    sd1 = 1,
    sd2 = sd1 * SDR
    ) 
conditions %>% select(1:5)


## ----Run simulation 3---------------------------------------------------------------------------------------------------------
set.seed(7913)
i = 10000
tic()
sims = map_dfr(1:i, ~ conditions) %>% # each condition 10,000 times
  rowid_to_column(var = "sim") %>%
  rowwise() %>%
  mutate(p.value = sim_ttest(n = n, M_diff = M_diff, GR = GR, SDR = SDR))
toc()


## ----Result plot Power by GR and SDR 1----------------------------------------------------------------------------------------
sims %>% 
  group_by(GR, SDR) %>% 
  summarise(P_p05 = mean(p.value < 0.05)) %>% 
  ggplot(aes(factor(GR), P_p05, group = 1)) + 
  geom_point() + geom_line() +
  facet_wrap(vars(SDR), labeller = label_both) +
  labs(x = "Group size ratio", y = "Power")


## ----Result plot Power by GR and SDR 2----------------------------------------------------------------------------------------
sims %>% 
  group_by(n, M_diff, GR, SDR, n1, n2, sd1, sd2) %>% 
  summarise(P_p05 = mean(p.value < 0.05), .groups = "drop") %>% 
  mutate(implies = str_glue("n_c = {n1}, n_t = {n2}, sd_c = {round(sd1, 2)}, sd_t = {round(sd2, 2)}")) %>% 
  mutate(implies = fct_reorder(implies, P_p05)) %>% 
  ggplot(aes(P_p05, implies)) + geom_point(size = 3) +
  labs(x = "Power", y = "Condition") +
  theme(axis.text.y = element_text(size = 20))


## ----Simulation function 4----------------------------------------------------------------------------------------------------
sim_ttest = function(n = 200, pop_sd = 1, GR = 1, SDR = 1, M_diff = 0) {
  n1 = round(n / (GR + 1))
  n2 = round(n1 * GR)
  sd1 = sqrt(
    ((pop_sd^2 - ((n1 * n2) / (n1 + n2)^2) * M_diff^2) * (n1 + n2)) / 
      (n1 + n2 * SDR^2)
    )
  sd2 = sd1 * SDR # sd2/sd1
  g1 = rnorm(n = n1, mean = 0, sd = sd1)
  g2 = rnorm(n = n2, mean = M_diff, sd = sd2)
  res = t.test(g1, g2)$p.value
  return(res)
}
sim_ttest(n = 300, M_diff = 0.3, GR = 0.5)


## ----Conditions 4-------------------------------------------------------------------------------------------------------------
conditions = expand_grid(GR = c(0.5, 1, 2), 
                         SDR = c(0.5, 1, 2),
                         pop_sd = 1,
                         n = 300,
                         M_diff = 0.3) %>% 
  rowid_to_column(var = "condition") %>% 
  mutate(
    n1 = round(n / (GR + 1)), n2 = round(n1 * GR),
    sd1 = sqrt(((pop_sd^2 - ((n1 * n2) / (n1 + n2)^2) * M_diff^2) * (n1 + n2)) / (n1 + n2 * SDR^2)),
    sd2 = sd1 * SDR
    ) 
conditions %>% select(1:6)


## ----Run simulation 4---------------------------------------------------------------------------------------------------------
set.seed(39)
i = 10000
tic()
sims = map_dfr(1:i, ~ conditions) %>% # each condition 10,000 times
  rowid_to_column(var = "sim") %>%
  rowwise() %>%
  mutate(p.value = sim_ttest(n = n, M_diff = M_diff, GR = GR, SDR = SDR))
toc()


## ----Result plot Power by GR and SDR 3----------------------------------------------------------------------------------------
sims %>% 
  group_by(GR, SDR) %>% 
  summarise(P_p05 = mean(p.value < 0.05)) %>% 
  ggplot(aes(factor(GR), P_p05, group = 1)) + 
  geom_point() + geom_line() +
  facet_wrap(vars(SDR), labeller = label_both) +
  labs(x = "Group size ratio", y = "Power")


## ----Result plot Power by GR and SDR 4----------------------------------------------------------------------------------------
sims %>% 
  group_by(n, M_diff, GR, SDR, n1, n2, sd1, sd2) %>% 
  summarise(P_p05 = mean(p.value < 0.05), .groups = "drop") %>% 
  mutate(implies = str_glue("n_c = {n1}, n_t = {n2}, sd_c = {round(sd1, 2)}, sd_t = {round(sd2, 2)}")) %>% 
  mutate(implies = fct_reorder(implies, P_p05)) %>% 
  ggplot(aes(P_p05, implies)) + geom_point(size = 3) +
  labs(x = "Power", y = "Condition") +
  theme(axis.text.y = element_text(size = 20))


## ----Simulation function adapted from Exercise 2a-----------------------------------------------------------------------------
## # We use almost the same simulation function,
## # but we omit Student's t-test, because we already showed its
## # problems with unequal group sizes and group SDs
## sim_ttest_glm = function(n = 200, GR = 1, lambda1 = 1, M_diff = 0) {
##   n1 = round(n / (GR + 1))
##   n2 = round(n1 * GR)
##   lambda2 = lambda1 + M_diff
##   g1 = rpois(n = n1, lambda = lambda1)
##   g2 = rpois(n = n2, lambda = lambda2)
##   Welch = t.test(g1, g2)$p.value
##   GLM = glm(outcome ~ group,
##             data = data.frame(outcome = c(g1, g2),
##                               group = c(rep("g1", n1), rep("g2", n2))),
##             family = poisson)
##   res = lst(Welch, GLM = coef(summary(GLM))[2, 4])
##   return(res)
## }


## ----Conditions I-------------------------------------------------------------------------------------------------------------
## # It makes sense to vary group size ratio, lambda, and M_diff
## # The positive relationship between n and power is trivial
## conditions = expand_grid(GR = c(0.5, 1, 2),
##                          n = 200,
##                          lambda1 = c(1, 10, 20),
##                          M_diff = c(0.3, 0.5, 0.8)) %>%
##   rowid_to_column(var = "condition")


## ----Conditions II------------------------------------------------------------------------------------------------------------
## conditions = expand_grid(GR = c(0.5, 1, 2),
##                          n = 200,
##                          lambda1 = c(1, 10, 20),
##                          d = c(0.2, 0.4, 0.6)) %>% # standardized mean difference.
##   mutate(M_diff = d * sqrt(lambda1)) %>% # absolute mean difference
##   rowid_to_column(var = "condition")


## ----Power plot I-------------------------------------------------------------------------------------------------------------
## sims %>%
##   group_by(GR, lambda1, M_diff, method) %>%
##   summarise(P_p05 = mean(p.value < 0.05)) %>%
##   ggplot(aes(factor(GR), P_p05, color = method, group = method)) +
##   geom_point() + geom_line() +
##   facet_grid(M_diff ~ lambda1, labeller = label_both) +
##   labs(x = "Group size ratio", y = "Power")

