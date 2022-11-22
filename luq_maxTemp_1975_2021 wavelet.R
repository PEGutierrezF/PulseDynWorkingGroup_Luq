




# ---------------------------------------------
# Maximum temperature at El Verde Field Station, Puerto Rico
# wavelet analysis
# Nov 22 2022
# Pablo E. Gutiérrez-Fonseca
# ---------------------------------------------
#  



rm(list=ls(all=TRUE)) 



All_var_1975_2021=read.csv("All_variables 1975-2021.csv")
tail(All_var_1975_2021)
summary(All_var_1975_2021)

maxTempLuq <- All_var_1975_2021 %>% select(date, max_temp, min_temp) %>% # select date, max and min
  mutate(max_temp_apro = na.approx(max_temp)) %>% # extrapolate max
  mutate(min_temp_apro = na.approx(min_temp)) # extrapolate min
summary(maxTempLuq)



# Select only Maximum Temperature -----------------------------------------

maxTempLuq_apro <- maxTempLuq %>% select(date, max_temp_apro) # let's work with max first
summary(maxTempLuq_apro)
head(maxTempLuq_apro)



# Wavelet Rainfall --------------------------------------------------------

Luq_temp <- analyze.wavelet(maxTempLuq_apro, "max_temp_apro", make.pval = TRUE, n.sim = 10)

wt.image(Luq_temp, main = "Luquillo-LTER Daily Precipitation",
         periodlab = "period (daily)",
         label.time.axis = T, show.date = T, date.format = "%Y-%m-%d",
         color.key = "quantile",legend.params = list(label.digits = 3, lab = "wavelet power levels", mar = 8))

LUQ_walvelet <- wt.image(Luq_temp, main = "Luquillo-LTER Daily Precipitation",
                         periodlab = "period (daily)",
                         label.time.axis = T, show.date = T, date.format = "%Y-%m-%d",
                         color.key = "quantile",legend.params = list(label.digits = 3, lab = "wavelet power levels", mar = 8),
                         plot.contour = FALSE)   # without contour lines
