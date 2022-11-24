## ----setup, include=FALSE-------------------------------------------------------------------------------
pacman::p_load(knitr, mgcv, tictoc, tidyverse)
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 25,
                                     axis_title_size = 25,
                                     strip_text_size = 20))


## -------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------
conditions = expand_grid(
  GR = 1, 
  SDR = 1,
  n = c(100, 200, 300),
  M_diff = c(0.2, 0.4, 0.6)) %>% 
  rowid_to_column(var = "condition")
conditions


## -------------------------------------------------------------------------------------------------------
set.seed(326)
i = 10000
tic()
sims = map_dfr(1:i, ~ conditions) %>% # each condition i times
  rowid_to_column(var = "sim") %>% # within simulation comparison
  rowwise() %>%
  mutate(p.value = list(sim_ttest(n = n, M_diff = M_diff))) %>% 
  unnest_longer(p.value, indices_to = "method")
toc()


## -------------------------------------------------------------------------------------------------------
sims %>% 
  filter(M_diff == 0.4 & method == "Student") %>% 
  group_by(n, M_diff, method) %>% 
  summarise(P_p05 = mean(p.value < 0.05)) %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), round, 3))


## -------------------------------------------------------------------------------------------------------
power.t.test(n = c(100, 200, 300)/2,
             delta = 0.4, sd = 1,
             sig.level = 0.05)


## -------------------------------------------------------------------------------------------------------
sims %>% 
  group_by(n, M_diff, method) %>% 
  summarise(P_p05 = mean(p.value < 0.05)) %>% 
  ggplot(aes(factor(n), P_p05, color = method, group = method)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) + 
  geom_line(position = position_dodge(width = 0.3), size = 1.5) +
  facet_wrap(vars(M_diff), labeller = label_both) +
  labs(x = "n", y = "Power")


## -------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------
conditions = expand_grid(GR = 1, 
                         SDR = 1,
                         n = seq(from = 50, to = 500, by = 10),
                         M_diff = 0.4) %>% 
  rowid_to_column(var = "condition")
i = 250
conditions = map_dfr(1:i, ~ conditions) %>%
  rowid_to_column(var = "sim")
conditions


## -------------------------------------------------------------------------------------------------------
i = 250*46 # same total number of simulations
set.seed(86)
conditions = tibble(sim = 1:i,
                    GR = 1, 
                    SDR = 1,
                    n = sample(x = 50:500, size = i, replace = TRUE),
                    M_diff = 0.4)
conditions


## -------------------------------------------------------------------------------------------------------
tic()
sims = conditions %>% 
  rowwise() %>%
  mutate(p.value = sim_ttest(n = n, M_diff = M_diff, GR = GR, SDR = SDR))
toc()


## -------------------------------------------------------------------------------------------------------
sims %>% 
  mutate(p05 = as.integer(p.value < 0.05)) %>% 
  ggplot(aes(n, p05)) +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cr"), 
              method.args = list(family = "binomial")) + 
  labs(y = "Power")


## -------------------------------------------------------------------------------------------------------
power_mod = sims %>% 
  mutate(p05 = as.integer(p.value < 0.05)) %>%
  gam(p05 ~ s(n, bs = "cr"), family = binomial, data = .)


## -------------------------------------------------------------------------------------------------------
tibble(n = c(120, 256, 404)) %>% 
  mutate(power = predict(power_mod, newdata = .,
                         type = "response"))


## -------------------------------------------------------------------------------------------------------
power.t.test(n = c(120, 256, 404)/2,
             delta = 0.4, sd = 1, 
             sig.level = 0.05)


## -------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------
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


## -------------------------------------------------------------------------------------------------------
set.seed(7913)
i = 10000
tic()
sims = map_dfr(1:i, ~ conditions) %>% # each condition 10,000 times
  rowid_to_column(var = "sim") %>%
  rowwise() %>%
  mutate(p.value = sim_ttest(n = n, M_diff = M_diff, GR = GR, SDR = SDR))
toc()


## -------------------------------------------------------------------------------------------------------
sims %>% 
  group_by(GR, SDR) %>% 
  summarise(P_p05 = mean(p.value < 0.05)) %>% 
  ggplot(aes(factor(GR), P_p05, group = 1)) + 
  geom_point() + geom_line() +
  facet_wrap(vars(SDR), labeller = label_both) +
  labs(x = "Group size ratio", y = "Power")


## -------------------------------------------------------------------------------------------------------
sims %>% 
  group_by(n, M_diff, GR, SDR, n1, n2, sd1, sd2) %>% 
  summarise(P_p05 = mean(p.value < 0.05), .groups = "drop") %>% 
  mutate(implies = str_glue("n_c = {n1}, n_t = {n2}, sd_c = {round(sd1, 2)}, sd_t = {round(sd2, 2)}")) %>% 
  mutate(implies = fct_reorder(implies, P_p05)) %>% 
  ggplot(aes(P_p05, implies)) + geom_point(size = 3) +
  labs(x = "Power", y = "Condition") +
  theme(axis.text.y = element_text(size = 20))


## -------------------------------------------------------------------------------------------------------
sim_ttest = function(n = 200, pooled_sd = 1, GR = 1, SDR = 1, M_diff = 0) {
  n1 = round(n / (GR + 1))
  n2 = round(n1 * GR)
  sd1 = sqrt(
    ((pooled_sd^2 - ((n1 * n2) / (n1 + n2)^2) * M_diff^2) * (n1 + n2)) / 
      (n1 + n2 * SDR^2)
    )
  sd2 = sd1 * SDR # sd2/sd1
  g1 = rnorm(n = n1, mean = 0, sd = sd1)
  g2 = rnorm(n = n2, mean = M_diff, sd = sd2)
  res = t.test(g1, g2)$p.value
  return(res)
}
sim_ttest(n = 300, M_diff = 0.3, GR = 0.5)


## -------------------------------------------------------------------------------------------------------
conditions = expand_grid(GR = c(0.5, 1, 2), 
                         SDR = c(0.5, 1, 2),
                         pooled_sd = 1,
                         n = 300,
                         M_diff = 0.3) %>% 
  rowid_to_column(var = "condition") %>% 
  mutate(
    n1 = round(n / (GR + 1)), n2 = round(n1 * GR),
    sd1 = sqrt(((pooled_sd^2 - ((n1 * n2) / (n1 + n2)^2) * M_diff^2) * (n1 + n2)) / (n1 + n2 * SDR^2)),
    sd2 = sd1 * SDR
    ) 
conditions %>% select(1:6)


## -------------------------------------------------------------------------------------------------------
set.seed(39)
i = 10000
tic()
sims = map_dfr(1:i, ~ conditions) %>% # each condition 10,000 times
  rowid_to_column(var = "sim") %>%
  rowwise() %>%
  mutate(p.value = sim_ttest(n = n, M_diff = M_diff, GR = GR, SDR = SDR))
toc()


## -------------------------------------------------------------------------------------------------------
sims %>% 
  group_by(GR, SDR) %>% 
  summarise(P_p05 = mean(p.value < 0.05)) %>% 
  ggplot(aes(factor(GR), P_p05, group = 1)) + 
  geom_point() + geom_line() +
  facet_wrap(vars(SDR), labeller = label_both) +
  labs(x = "Group size ratio", y = "Power")


## -------------------------------------------------------------------------------------------------------
sims %>% 
  group_by(n, M_diff, GR, SDR, n1, n2, sd1, sd2) %>% 
  summarise(P_p05 = mean(p.value < 0.05), .groups = "drop") %>% 
  mutate(implies = str_glue("n_c = {n1}, n_t = {n2}, sd_c = {round(sd1, 2)}, sd_t = {round(sd2, 2)}")) %>% 
  mutate(implies = fct_reorder(implies, P_p05)) %>% 
  ggplot(aes(P_p05, implies)) + geom_point(size = 3) +
  labs(x = "Power", y = "Condition") +
  theme(axis.text.y = element_text(size = 20))

