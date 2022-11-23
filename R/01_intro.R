## ----setup, include=FALSE---------------------------------------------------------------------------
pacman::p_load(knitr, mgcv, hrbrthemes, extraDistr, tictoc, tidyverse)
set.seed(376)


## ---- echo=TRUE-------------------------------------------------------------------------------------
sample(x = 30, size = 4)


## ---------------------------------------------------------------------------------------------------
tibble(Package = c("R", sort(c(pacman::p_loaded())))) %>% 
  mutate(Version = map(Package, ~ as.character(pacman::p_ver(.x)))) %>% 
  str_glue_data("{Package} (Vers. {Version})") %>% 
  str_c(collapse = ", ") %>% 
  cat()

