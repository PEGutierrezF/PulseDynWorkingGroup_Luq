



# ---------------------------------------------
# Threshold for several variables
# 22 Sep 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  




###########################################################################
# Temperature El Verde Field Station --------------------------------------
###########################################################################


. <- All_var_1975_2021 %>% select(max_temp, min_temp) %>%
  filter(!is.na(max_temp)) %>%
  filter(!is.na(min_temp))


.. <- (.$max_temp - .$min_temp)
mean(..)
min(..)
max(..)
which(.. == 17.2, arr.ind=TRUE)


min(.)
max(.)

###########################################################################
