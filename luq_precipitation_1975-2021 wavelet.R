



# ---------------------------------------------
# Precipitation El Verde Field Station 1975-2021
# wavelet analysis
# 22 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



rm(list=ls(all=TRUE)) #give R a blank slate



setwd("D:/Curriculum/14_ Colaboracion/2021 Pulse LTER/Pulse WG PR/PulseDynWorkingGroup_Luq/data")
rain <- read.csv("evra1975-2021.csv")
head(rain)
summary(rain)

rainLuq <- rain %>% select(date,rainfall) %>%
  filter(!is.na(date)) %>%
  filter(!is.na(rainfall))

summary(rainLuq)
head(rainLuq)


# Wavelet Rainfall --------------------------------------------------------

rain_wt <- analyze.wavelet(rainLuq, "rainfall", make.pval = TRUE, n.sim = 10)

wt.image(rain_wt, main = "Luquillo-LTER Daily Precipitation",
         periodlab = "period (daily)",
         label.time.axis = T, show.date = T, date.format = "%Y-%m-%d",
         color.key = "quantile",legend.params = list(label.digits = 3, lab = "wavelet power levels", mar = 8))

LUQ_walvelet <- wt.image(rain_wt, main = "Luquillo-LTER Daily Precipitation",
         periodlab = "period (daily)",
         label.time.axis = T, show.date = T, date.format = "%Y-%m-%d",
         color.key = "quantile",legend.params = list(label.digits = 3, lab = "wavelet power levels", mar = 8),
         plot.contour = FALSE)   # without contour lines





# time-averaged cross-wavelet power
wt.avg(rain_wt, siglvl = 0.01, sigcol = "red", sigpch = 20,
       periodlab = "period (days)")


