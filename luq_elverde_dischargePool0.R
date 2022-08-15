



# ---------------------------------------------
# Discharge Pool 0. El Verde Field Station
# Pulse metrics using Fourier time-series analyses
# 15 Aug 2022
# Pablo E. Guti�rrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  


rm(list=ls(all=TRUE)) #give R a blank slate

setwd("D:/Curriculum/14_ Colaboracion/2021 Pulse LTER/Pulse WG PR/PulseDynWorkingGroup_Luq/data")
discharge=read.csv("PrietaDischarge_15min_2006-2020.csv", sep=",", header=TRUE)
head(discharge)
summary(discharge)

# Calculate dayly mean Pool 0, Prieta
disc_Pool0 <- discharge %>%
  group_by(date) %>%
  summarize(Prieta_pool0 = mean(Discharge_FSN, na.rm = TRUE),
            daily_discharge_max = max(Discharge_FSN, na.rm = TRUE))

# write.csv(disc_Pool0, "mean and max summary.csv")

. <- disc_Pool0 %>% 
  select(date, Prieta_pool0)

disc_Pool0_mean <- as.data.frame(.)
write.csv(disc_Pool0_mean, "disc_Pool0_mean.csv")
write.table(disc_Pool0_mean,"newdf.txt",sep="\t",row.names=FALSE)



# Real analysis -----------------------------------------------------------

### Fourier Analysis & Data Extremes ###
pool0.data = read.csv("disc_Pool0_mean.csv", header=TRUE)[,1:2]
summary(pool0.data)

### ISO date format ###
pool0.data$date<-as.POSIXct(pool0.data$date,"%Y-%m-%d",tz = "UTC") 
summary(pool0.data)

head(pool0.data)
tail(pool0.data)

#Create 'streamflow' object
pool0.data <- asStreamflow(pool0.data, start.date="2006-06-28",
                           end.date="2020-06-04",
                           river.name="Prieta_pool0")

###Plot data to visually inspect time-series pulses
luq <- pool0.data$data
plot(luq$discharge~luq$date, type="l", ylab="Water Level (NAVD88 + 50, cm)", 
                    xlab="Date")

#Fourier Analysis
luq.seas<-fourierAnalysis(pool0.data)
summary(luq.seas) # SUMMARY STATS
#1)"noise color” and is a measure of flashiness. Noise color ranges from 0 (white noise with no autocorrelation—highly flashy) to values
#over 2 (reddened, values with strong autocorrelation in which high- or low- events persist over the short term).
#2)"signal-noise ratio" is a measure of seasonality (higher is more seasonal).
#3) "average" is long-term ordinal day average.
plot(luq.seas, plot.type="hydrograph") # PLOT


#Calculate annual extremes
luq.extremes<-annualExtremes(pool0.data)
names(luq.extremes)


pool0.data<-read.table("disc_Pool0_mean.csv", sep=",", header=TRUE)[,1:2]
summary(pool0.data)
luq.allstatss <- allstats(file.name ="disc_Pool0_mean.csv", 
                       river.name = "elverde_Pool0", 
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
median.year<-tapply(luq$discharge, list(luq$year), median)
median.sum<-median(median.year) #76.8
median.jday<-tapply(luq$discharge, list(luq$jday), median)
jday<-tapply(luq$jday, list(luq$jday), mean)
year<-tapply(luq$year, list(luq$year), mean)



### Long-term min and max per year ###
max.annual<-tapply(luq$discharge, list(luq$year), max)
min.annual<-tapply(luq$discharge, list(luq$year), min)

###Phenologies to assess mean date and CV
###min<- lm(jday~year) ### slope and p-value could indicate pheno-shifts (pulse metric)
###max<- lm(jday~year) ### slope and p-value could indicate pheno-shifts (pulse metric)
###days.min and days.max could assess the pulse phenology and character
###max.jday$annual.max - min.jday$annual.min
min<-lm(min.annual~year)
summary(min)
#plot(min) #use to check residuals
plot(min.annual~year)

max<-lm(max.annual~year)
summary(max)
#plot(max) #use to check residuals
plot(max.annual~year)

f1<-data.frame(luq.extremes$annual.max)
max.window<-(max(f1$jday)-min(f1$jday))
###Maximum period of peak water level period 341 days###

f2 <- data.frame(luq.extremes$annual.min)
min.window<-(max(f2$jday)-min(f2$jday))
###Maximum period of low water level period 364 days###


### Plot annual.min and annual.max values ###
par(mfrow=c(1,2), mar = c(5, 5, 1, 1))
plot(f1$discharge~f1$jday, ylab="Water Level (NAVD88 + 50, cm)", xlab="Julian Day", ylim=c(0,150), xlim=c(0,360))
par(new=TRUE)
plot(jday, median.jday, type="l", ylim=c(0,150), xlim=c(0,360), ylab="", xlab="")
abline(76.8,0)

plot(f2$discharge~f2$jday, ylab="Water Level (NAVD88 + 50, cm)", xlab="Julian Day", ylim=c(0,150), xlim=c(0,360))
par(new=TRUE)
plot(jday, median.jday, type="l", ylim=c(0,150), xlim=c(0,360), ylab="", xlab="")
abline(76.8,0)




####################################3
#name your LTER
lter<-"LUQ"

#name your site
site<-"Prieta Pool 0"

#name your driver
driver<-"water level"
#units = units of driver (e.g., mm)
units<-"cubic feet per second"

#maximum window
max<-max.window
max.units<-"days"

#minimum window
min<-min.window
min.units<-"days"





######### Save metrics into data frame ##################
pulse_metrics_FCE<-data.frame(lter,site,driver,units,max,max.units,min,min.units,fce.allstats)

write.csv(pulse_metrics_FCE,"pulse_metrics_FCE.csv")
#then we will merge metrics data frame across LTER datasets using rbind
