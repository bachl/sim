## ----setup----------------------------------------------------------------------------------------------
pacman::p_load(knitr, tictoc, tidyverse)
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 25,
                                     axis_title_size = 25,
                                     strip_text_size = 20))


## ----One simulation: Parameters-------------------------------------------------------------------------
set.seed(28)
n = 150 # fixed total sample size
GR = 2 # n2/n1
n1 = round(n / (GR + 1))
n2 = round(n1 * GR)
sd1 = 1
SDR = 2  # sd2/sd1
sd2 = sd1 * SDR


## ----One simulation: *t*-tests--------------------------------------------------------------------------
g1 = rnorm(n = n1, mean = 0, sd = sd1)
g2 = rnorm(n = n2, mean = 0, sd = sd2)
t.test(g1, g2) # Welch (default)
t.test(g1, g2, var.equal = TRUE) # Student


## ----Inspect the output of t.test()---------------------------------------------------------------------
t.test(g1, g2) %>% str()


## ----Extract p-value from t.test()----------------------------------------------------------------------
t.test(g1, g2)$p.value


## ----Wrap it into a function----------------------------------------------------------------------------
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


## ----Call the function once-----------------------------------------------------------------------------
sim_ttest(GR = 2, SDR = 0.5) # Implies n1 = 50, n2 = 100, sd1 = 1, sd2 = 0.5


## ----Setup of experimental conditions: expand_grid example----------------------------------------------
expand_grid(a = 1:2, b = 3:4)


## ----Setup of experimental conditions-------------------------------------------------------------------
conditions = expand_grid(GR = c(1, 2),
                         SDR = c(0.5, 1, 2)) %>% 
  rowid_to_column(var = "condition")
conditions


## ----Setup of experimental conditions 2-----------------------------------------------------------------
conditions %>% 
  mutate(implies = str_glue("n1 = {round(150 / (GR + 1))}, n2 = {round(150 - 150 / (GR + 1))}, sd1 = 1, sd2 = {1 * SDR}")) %>% 
  knitr::kable()


## ----Run simulation experiment--------------------------------------------------------------------------
set.seed(689)
i = 10000 # number of sim runs per condition
tic() # simple way to measure wall time
sims = map_dfr(1:i, ~ conditions) %>% # each condition i times
  rowid_to_column(var = "sim") %>% # within simulation comparison
  rowwise() %>%
  mutate(p.value = list(sim_ttest(GR = GR, SDR = SDR))) %>% 
  unnest_longer(p.value, indices_to = "method")
toc() # simple way to measure wall time


## ----Inspect simulated data-----------------------------------------------------------------------------
sims


## ----Results: Histogram of p-values---------------------------------------------------------------------
sims %>% 
  ggplot(aes(p.value)) +
  geom_histogram(binwidth = 0.05, boundary = 0) + 
  facet_grid(method ~ GR + SDR, labeller = label_both) + 
  scale_x_continuous(breaks = c(1, 3)/4)


## ----Results: Q-Q-plot of p-values----------------------------------------------------------------------
sims %>% 
  ggplot(aes(sample = p.value, color = method)) +
  geom_qq(distribution = stats::qunif, size = 0.2) + 
  geom_abline(slope = 1) + 
  facet_grid(GR ~ SDR, labeller = label_both)


## ----Results: Looking at the relevant tail--------------------------------------------------------------
sims %>% 
  group_by(GR, SDR, method) %>% 
  summarise(P_p001 = mean(p.value <= 0.001),
            P_p01 = mean(p.value <= 0.01), 
            P_p05 = mean(p.value <= 0.05)) %>% 
  kable(digits = 3)


## ----Sample from a Poisson distribution-----------------------------------------------------------------
## rpois(n = n1, lambda = lambda1)


## ----Poisson regression---------------------------------------------------------------------------------
## glm(outcome ~ group, data = data, family = poisson)


## ----Data format for Poisson regression-----------------------------------------------------------------
## data.frame(outcome = c(g1, g2),
##            group = c(rep("g1", n1), rep("g2", n2)))


## ----Extract p-value from fitted Poisson regression object----------------------------------------------
## GLM = glm(outcome ~ group, data = data, family = poisson)
## coef(summary(GLM))[2, 4]


## ----Complete simulation function-----------------------------------------------------------------------
## # No standard deviation ratio any more because SD fixed at sqrt(lambda)
## # M_diff not necessary at this stage, but useful later
## sim_ttest_glm = function(n = 200, GR = 1, lambda1 = 1, M_diff = 0) {
##   n1 = round(n / (GR + 1))
##   n2 = round(n1 * GR)
##   lambda2 = lambda1 + M_diff
##   g1 = rpois(n = n1, lambda = lambda1)
##   g2 = rpois(n = n2, lambda = lambda2)
##   Welch = t.test(g1, g2)$p.value
##   Student = t.test(g1, g2, var.equal = TRUE)$p.value
##   GLM = glm(outcome ~ group,
##             data = data.frame(outcome = c(g1, g2),
##                               group = c(rep("g1", n1), rep("g2", n2))),
##             family = poisson)
##   res = lst(Welch, Student, GLM = coef(summary(GLM))[2, 4])
##   return(res)
## }


## ----Conditions-----------------------------------------------------------------------------------------
## # It makes sense to vary group size ratio and lambda
## conditions = expand_grid(GR = c(0.5, 1, 2),
##                          n = 600,
##                          lambda1 = c(1, 10, 20),
##                          M_diff = 0) %>%
##   rowid_to_column(var = "condition")


## ----Run simulation-------------------------------------------------------------------------------------
## # use smaller i for now because additional glm() needs time
## set.seed(35)
## i = 1000
## tic()
## sims = map_dfr(1:i, ~ conditions) %>%
##   rowid_to_column(var = "sim") %>%
##   rowwise() %>%
##   mutate(p.value = list(sim_ttest_glm(n = n, M_diff = M_diff,
##                                  GR = GR, lambda1 = lambda1))) %>%
##   unnest_longer(p.value, indices_to = "method")
## toc()


## ----Plots----------------------------------------------------------------------------------------------
## # Histogram of p-values
## sims %>%
##   ggplot(aes(p.value)) +
##   geom_histogram(binwidth = 0.1, boundary = 0) +
##   facet_grid(method ~ lambda1 + GR, labeller = label_both) +
##   scale_x_continuous(breaks = c(1, 3)/4)
## # QQ-Plot of p-values
## sims %>%
##   ggplot(aes(sample = p.value, color = method)) +
##   geom_qq(distribution = stats::qunif, geom = "line") +
##   geom_abline(slope = 1) +
##   facet_grid(GR ~ lambda1, labeller = label_both)
## # Rates of p < .05
## sims %>%
##   group_by(GR, lambda1, method) %>%
##   summarise(P_p05 = mean(p.value < 0.05)) %>%
##   ggplot(aes(factor(GR), P_p05, color = method, group = method)) +
##   geom_point() + geom_line() +
##   geom_hline(yintercept = 0.05, linetype = 2) +
##   facet_wrap(vars(lambda1), labeller = label_both) +
##   labs(x = "Group size ratio", y = "Proportion p < 0.05")

