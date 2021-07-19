



# ---------------------------------------------
# Discharge Pool 0
# 22 Jun 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



rm(list=ls(all=TRUE)) #give R a blank slate

setwd("D:/Curriculum/14_ Colaboracion/2021 Pulse LTER/Pulse WG PR/PulseDynWorkingGroup_Luq/data")
discharge=read.csv("PrietaDischarge_15min_2006-2020.csv")
head(discharge)
summary(discharge)


disc_Pool0 <- discharge %>%
  group_by(date) %>%
  summarize(daily_discharge_mean = mean(Discharge_FSN, na.rm = TRUE),
            daily_discharge_max = max(Discharge_FSN, na.rm = TRUE))

max(disc_Pool0$daily_discharge)

# write.csv(disc_Pool0, "mean and max summary.csv")



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



# Wavelet Discharge Pool 0 ------------------------------------------------

discharge.wt <- analyze.wavelet(disc_Pool0, "daily_discharge",make.pval = TRUE, n.sim = 10) 

wt.image(discharge.wt, main = "Luquillo-LTER Daily Discharge Pool 0",
         periodlab = "period (daily)",
         label.time.axis = T, show.date = T, date.format = "%Y-%m-%d",
         color.key = "quantile",legend.params = list(label.digits = 3, lab = "wavelet power levels", mar = 8))


