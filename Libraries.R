



# ---------------------------------------------
# Upload libraries
# 22 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



install.packages('gganimate')

libraries <- c("tidyverse", "ggplot2", "xts","emmeans",
               'quantmod',"ggpubr","broom","gganimate","GmAMisc",
               "WaveletComp", "dplyr", "matrixStats", "tidyr", 
               "lubridate", "readr")

lapply(libraries, require, character.only = TRUE)



