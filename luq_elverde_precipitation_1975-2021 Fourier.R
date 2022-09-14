



# ---------------------------------------------
# Precipitation El Verde Field Station 1975-2021
# Pulse metrics using Fourier time-series analyses
# 12 Sep 2022
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
tail(rainLuq)

# Step 1 ------------------------------------------------------------------

#positivize the data for rainfall by adding 20, cannot take log of zero/negative
rainLuq$rainfall <- rainLuq$rainfall+20


# Step 2 ------------------------------------------------------------------

rainLuq$date <-as.POSIXct(rainLuq$date,"%Y-%m-%d",tz = "UTC")
summary(rainLuq)

# Step 3 ------------------------------------------------------------------

### Fourier Analysis & Data Extremes ###
#Create 'streamflow' object
rainLuq <- asStreamflow(rainLuq, start.date="1975-01-01",
                        end.date="2021-03-30", 
                        river.name="El Verde")


# Step 4 ------------------------------------------------------------------

###Plot data to visually inspect time-series pulses
#rainLuq <- rainLuq$data
#plot(rainLuq$discharge~rainLuq$date, type="l", 
#     ylab="Precipitation (mm)", xlab="Date")



# Step 5 ------------------------------------------------------------------

#Fourier Analysis
rainLuq.fourier <- fourierAnalysis(rainLuq)
rainLuq.fourier
summary(rainLuq.fourier) # SUMMARY STATS
#1)"noise color" and is a measure of flashiness. Noise color ranges from 0 (white noise with no autocorrelation-highly flashy) to values
#over 2 (reddened, values with strong autocorrelation in which high- or low- events persist over the short term).
#2)"signal-noise ratio" is a measure of seasonality (higher is more seasonal).
#3) "average" is long-term ordinal day average.
plot(rainLuq.fourier, plot.type="hydrograph") # PLOT


# Step 6 ------------------------------------------------------------------

#Calculate annual extremes
rainLuq.extremes<-annualExtremes(rainLuq)
names(rainLuq.extremes)


# compare for periods from 2002 to 2011 and 2012 to 2021
comp = compare.periods(c("2002-01-01", "2011-12-31"),
                       c("2012-01-01", "2021-01-31"), rainLuq, plot=T)


# Step 7. Export results --------------------------------------------------

# allstats function works on files
# write R 'streamflow' object data into csv file
rainLuq <- rainLuq$data
write.csv(rainLuq, "RainLuq.csv")

RainLuq.allstats <- allstats(file.name = "evra1975-2021.csv", river.name = "El Verde", 
                             file.type="csv", date.col=1,
                             discharge.col=2, skipped.rows=0)

#A data frame with columns
#a.rms = Root mean squared amplitude.
#n.rms = Root mean squared noise.
#snr = Signal-to-noise ratio.
#theta.d = Daily noise color.
#theta.a = Annual noise color.
#sigma.lf = Sigma for low flow events.
#sigma.hf = Sigma for high flow events.
#q2 = 2-year return level (flood).
#q10 = 10-year return level (flood).
#l2 = 2-year return level (drought).
#l10 = 10-year return level (drought).



### Long-term median values per Julian day ###
median.year <- tapply(rainLuq$discharge, list(rainLuq$year), median)
median.sum <- median(median.year) #40.415
median.jday <- tapply(rainLuq$discharge, list(rainLuq$jday), median)
jday <- tapply(rainLuq$jday, list(rainLuq$jday), mean)
year <- tapply(rainLuq$year, list(rainLuq$year), mean)

### Long-term min and max per year ###
max.annual <- tapply(rainLuq$discharge, list(rainLuq$year), max)
min.annual <- tapply(rainLuq$discharge, list(rainLuq$year), min)

###Phenologies to assess mean date and CV
###min<- lm(jday~year) ### slope and p-value could indicate pheno-shifts (pulse metric)
###max<- lm(jday~year) ### slope and p-value could indicate pheno-shifts (pulse metric)
###days.min and days.max could assess the pulse phenology and character
###max.jday$annual.max - min.jday$annual.min
min<-lm(min.annual~year)
summary(min)
min.slope<--1.356e-03
min.p<-0.09011



Voy por aqui

#Quantile regression
min.annual<-as.vector(min.annual)
year<-as.vector(year)
min.rq<-rq(min.annual~year)
summary(min.rq)
min.rq.slope<-0.008

#plot(min) #use to check residuals
plot(min.annual~year)

max<-lm(max.annual~year)
max<-rq(max.annual~year)
summary(max)
max.slope<-0.03758
max.p<-0.204

#Quantile regression
max.annual<-as.vector(max.annual)
year<-as.vector(year)
max.rq<-rq(max.annual~year)
summary(max.rq)
max.rq.slope<-0.032

#plot(max) #use to check residuals
plot(max.annual~year)



f1<-data.frame(sev42Tmax.extremes$annual.max)
max.window<-(max(f1$jday)-min(f1$jday))
###Maximum period in-between peak Tmax period 53 days###

f2<-data.frame(sev42Tmax.extremes$annual.min)
min.window<-(max(f2$jday)-min(f2$jday))
###Maximum period in-between low Tmax period 364 days###


### Plot annual.min and annual.max values ###
par(mfrow=c(1,2), mar = c(5, 5, 1, 1))
plot(f1$discharge~f1$jday, ylab="Temperature max +10 (C)", xlab="Julian Day", ylim=c(0,60), xlim=c(0,360))
par(new=TRUE)
plot(jday, median.jday, type="l", ylim=c(0,60), xlim=c(0,360), ylab="", xlab="")
abline(40.415,0)

plot(f2$discharge~f2$jday, ylab="Temperature max +10 (C)", xlab="Julian Day", ylim=c(0,60), xlim=c(0,360))
par(new=TRUE)
plot(jday, median.jday, type="l", ylim=c(0,60), xlim=c(0,360), ylab="", xlab="")
abline(40.415,0)



####################################3
#name your LTER
lter<-"Luq"

#name your site
site<-"El Verde"

#name your driver
driver<-"Tmax"
#units = units of driver (e.g., mm)
units<-"C"

#maximum window
max<-max.window
#max.units<-"days"

#minimum window
min<-min.window
#min.units<-"days"

#maximum slope
max.slope<-max.slope

#maximum slope P-value
max.p<-max.p

#miniimum slope
min.slope<-min.slope

#minimum slope P-value
min.p<-min.p

######### Save metrics into data frame ##################
pulse_metrics_SEV42<-data.frame(lter,site,driver,units,max,max.units,min,min.units,max.slope,max.p,min.slope,min.p,sev42Tmax.allstats)

write.csv(pulse_metrics_SEV42,"pulse_metrics_SEV42.csv")
#then we will merge metrics data frame across LTER datasets using rbind
