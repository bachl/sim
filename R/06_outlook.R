## ----setup, include=FALSE---------------------------------------------------------------------------
pacman::p_load(knitr, tictoc, extraDistr, tidyverse)
theme_set(hrbrthemes::theme_ipsum_rc(base_size = 25,
                                     axis_title_size = 25,
                                     strip_text_size = 20))

# options(scipen = 999, digits = 2)
set.seed(376)

