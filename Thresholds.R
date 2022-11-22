



# ---------------------------------------------
# Threshold for several variables
# 22 Sep 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# This is the mean temperature change
5.492025 (5.5)

###########################################################################
# Temperature El Verde Field Station --------------------------------------
###########################################################################

All_var_1975_2021 <- read.csv("All_variables 1975-2021.csv")

. <- All_var_1975_2021 %>% select(max_temp, min_temp) %>%
  filter(!is.na(max_temp)) %>%
  filter(!is.na(min_temp))


.. <- (.$max_temp - .$min_temp)
mean(..) # as a threshold select this temperature
min(..)
max(..)

which(. == 36.7, arr.ind=TRUE)


min(.)
max(.)

###########################################################################
