



# ---------------------------------------------
# Upload libraries
# 22 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



libraries <- c("tidyverse", "ggplot2", "xts","emmeans",
               'quantmod',"ggpubr","broom","gganimate","GmAMisc",
               "WaveletComp", "dplyr", "matrixStats", "tidyr", 
               "lubridate", "readr",'discharge')

lapply(libraries, require, character.only = TRUE)

### Install Libraries ###
install.packages('discharge_1.0.tar.gz', 
                 lib='destination_directory',repos = NULL)
library(discharge)


