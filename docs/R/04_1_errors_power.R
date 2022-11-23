## ----setup, include=FALSE---------------------------------------------------------------------------
pacman::p_load(knitr, tictoc, tidyverse)
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 25,
                                     axis_title_size = 25,
                                     strip_text_size = 20))


## ---------------------------------------------------------------------------------------------------
set.seed(28)
n = 150 # fixed total sample size
GR = 2 # n2/n1
n1 = round(n / (GR + 1))
n2 = round(n1 * GR)
sd1 = 1
SDR = 2  # sd2/sd1
sd2 = sd1 * SDR


## ---------------------------------------------------------------------------------------------------
g1 = rnorm(n = n1, mean = 0, sd = sd1)
g2 = rnorm(n = n2, mean = 0, sd = sd2)
t.test(g1, g2) # Welch (default)
t.test(g1, g2, var.equal = TRUE) # Student


## ---------------------------------------------------------------------------------------------------
t.test(g1, g2) %>% str()


## ---------------------------------------------------------------------------------------------------
t.test(g1, g2)$p.value


## ---------------------------------------------------------------------------------------------------
sim_ttest = function(GR = 1, SDR = 1, n = 150) {
  n1 = round(n / (GR + 1))
  n2 = round(n1 * GR)
  sd1 = 1
  sd2 = sd1 * SDR
  g1 = rnorm(n = n1, mean = 0, sd = sd1)
  g2 = rnorm(n = n2, mean = 0, sd = sd2)
  welch = t.test(g1, g2)$p.value
  student = t.test(g1, g2, var.equal = TRUE)$p.value
  res = list("Welch" = welch, "Student" = student)
  return(res)
}


## ---------------------------------------------------------------------------------------------------
sim_ttest(GR = 2, SDR = 0.5) # Implies n1 = 50, n2 = 100, sd1 = 1, sd2 = 0.5


## ---------------------------------------------------------------------------------------------------
expand_grid(a = 1:2, b = 3:4)


## ---------------------------------------------------------------------------------------------------
conditions = expand_grid(GR = c(1, 2),
                         SDR = c(0.5, 1, 2)) %>% 
  rowid_to_column(var = "condition")
conditions


## ---------------------------------------------------------------------------------------------------
conditions %>% 
  mutate(implies = str_glue("n1 = {round(150 / (GR + 1))}, n2 = {round(150 - 150 / (GR + 1))}, sd1 = 1, sd2 = {1 * SDR}")) %>% 
  knitr::kable()


## ---------------------------------------------------------------------------------------------------
set.seed(689)
i = 10000 # number of sim runs per condition
tic() # simple way to measure wall time
sims = map_dfr(1:i, ~ conditions) %>% # each condition i times
  rowid_to_column(var = "sim") %>% # within simulation comparison
  rowwise() %>%
  mutate(p.value = list(sim_ttest(GR = GR, SDR = SDR))) %>% 
  unnest_longer(p.value, indices_to = "method")
toc() # simple way to measure wall time


## ---------------------------------------------------------------------------------------------------
sims


## ---------------------------------------------------------------------------------------------------
sims %>% 
  ggplot(aes(p.value)) +
  geom_histogram(binwidth = 0.05, boundary = 0) + 
  facet_grid(method ~ GR + SDR, labeller = label_both) + 
  scale_x_continuous(breaks = c(1, 3)/4)


## ---------------------------------------------------------------------------------------------------
sims %>% 
  ggplot(aes(sample = p.value, color = method)) +
  geom_qq(distribution = stats::qunif, size = 0.2) + 
  geom_abline(slope = 1) + 
  facet_grid(GR ~ SDR, labeller = label_both)


## ---------------------------------------------------------------------------------------------------
sims %>% 
  group_by(GR, SDR, method) %>% 
  summarise(P_p001 = mean(p.value <= 0.001),
            P_p01 = mean(p.value <= 0.01), 
            P_p05 = mean(p.value <= 0.05)) %>% 
  kable(digits = 3)

