



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

allNA=read.csv("physicochemistry.csv")
head(allNA)
summary(allNA)

dischargeNew <- dischargeNA %>% select(week,discharge) %>%
  filter(!is.na(week)) %>%
  filter(!is.na(discharge))


dischargeNew$week <-as.POSIXct(dischargeNew$week,"%Y-%m-%d",tz = "UTC")

discharge.wt <- analyze.wavelet(dischargeNew, "discharge",make.pval = TRUE, n.sim = 10) 

wt.image(discharge.wt, main = "Luquillo-LTER weekly Discharge, Pool 0 (2000-2017)",
                         periodlab = "period (week)",
                         label.time.axis = T, show.date = T, date.format = "%Y-%m-%d",
                         color.key = "quantile",legend.params = list(label.digits = 3, lab = "wavelet power levels", mar = 8),
                         plot.contour = FALSE)   # without contour lines


# Temperature -------------------------------------------------------------

allNA=read.csv("physicochemistry.csv")
head(allNA)
summary(allNA)

TemperatureNew <- allNA %>% select(week,Temperature) %>%
  filter(!is.na(week)) %>%
  filter(!is.na(Temperature))

TemperatureNew$week <-as.POSIXct(TemperatureNew$week,"%Y-%m-%d",tz = "UTC")

Temperature.wt <- analyze.wavelet(TemperatureNew, "Temperature", make.pval = TRUE, n.sim = 10) 

wt.image(Temperature.wt, main = "Luquillo-LTER weekly water Temperature, Pool 0 (2000-2017)",
         periodlab = "period (week)",
         label.time.axis = T, show.date = T, date.format = "%Y-%m-%d",
         color.key = "quantile",legend.params = list(label.digits = 3, lab = "wavelet power levels", mar = 8),
         plot.contour = FALSE)   # without contour lines



# DOC -------------------------------------------------------------

allNA=read.csv("physicochemistry.csv")
head(allNA)
summary(allNA)

DOCNew <- allNA %>% select(week,DOC) %>%
  filter(!is.na(week)) %>%
  filter(!is.na(DOC))

DOCNew$week <-as.POSIXct(DOCNew$week,"%Y-%m-%d",tz = "UTC")

DOC.wt <- analyze.wavelet(DOCNew, "DOC", make.pval = TRUE, n.sim = 10) 

wt.image(DOC.wt, main = "Luquillo-LTER weekly DOC, Pool 0 (2000-2017)",
         periodlab = "period (week)",
         label.time.axis = T, show.date = T, date.format = "%Y-%m-%d",
         color.key = "quantile",legend.params = list(label.digits = 3, lab = "wavelet power levels", mar = 8),
         plot.contour = FALSE)   # without contour lines

