



# ---------------------------------------------
# Upload libraries
# 22 Jun 2021
# Pablo E. Guti�rrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



libraries <- c("tidyverse", "ggplot2", "xts","emmeans",
               'quantmod',"ggpubr","broom","gganimate0","GmAMisc")
lapply(libraries, require, character.only = TRUE)


libraries<- c("WaveletComp", "dplyr", "matrixStats", "tidyr", "lubridate", "readr")
lapply(libraries, require, character.only = TRUE)




