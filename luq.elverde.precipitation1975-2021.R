



# ---------------------------------------------
# Precipitation El Verde Field Station 1975-2021
# Peaks and Valleys with "quantmod" packages
# 21 Jun 2021
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


# Step 1 ------------------------------------------------------------------

#make date data into ISO format
rainLuq$date <-as.POSIXct(rainLuq$date,"%Y-%m-%d",tz = "UTC")
summary(rainLuq)

#convert to xts (time series) object !!! assumes records of observations 
# are ordered by time !!!
# this function sorts by date so the records are in the correct order 
# prior to analysis
Luqxts <- xts(rainLuq[,-1], order.by=rainLuq[,1])


# a few functions to check attributes
index(Luqxts)
str(Luqxts)
tzone(Luqxts)
nmonths(Luqxts)
nyears(Luqxts)


######## Apply quantmod to raw data ########
#### make a quantmod graphic
chartSeries(Luqxts,theme="white")


#look at one year only
chartSeries(Luqxts, subset='2018::2019-01',theme="white")
#look at monthly deviations for last 10 years
chartSeries(to.monthly(Luqxts),up.col='blue',dn.col='red',subset='2009::2019-01',theme="white")

mean(Luqxts)
min(Luqxts)
max(Luqxts)
sd(Luqxts)

# Define threshold as peaks distributed >98%. So, 62mm
quantile(Luqxts,0.98) 

length(which(rainLuq$rainfall > 62))
length(rainLuq$rainfall [rainLuq$rainfall >62])

hist(Luqxts)
#### find peaks and valleys
#A peak[valley] is defined as the highest[lowest] value in a series, 
#so, the function can only define it after a change in direction has occurred. 
#This means that the function will always return the first period after the peak/valley
#of the timeseries, so as not to accidentally induce a look-ahead bias.
#So in every case you need to subtract 1 to plot the actual peak or valley
#The identification of peak/valley depends on the values in the immediately preceding time series.

#thresh is the threshhold, you should explore different threshold levels for your dataset
#it is important to go into this with a biologically informed threshold value
#e.g., for SEV, 20 mm of precip is considered a large event that can drive plant growth
#anything less than that threshold (a deviation of 20 mm) could be argued to be a less biologically meaningful pulse



# Step 2 ------------------------------------------------------------------
# set a threshold here:
threshold.peak <- 62


# Step 3 ------------------------------------------------------------------
# Function to identify peaks[valleys]
LUQpeaks<-findPeaks(Luqxts, thresh=threshold.peak)
plot(Luqxts[LUQpeaks-1])


# GRAPH PEAKS
#turn peaks into a dataframe to add it to a ggplot of the raw data
#and calculate metrics
#here we use 62 threshold
peaks <- as.data.frame(Luqxts[LUQpeaks-1])
head(peaks)

#plot peaks onto raw data for precipitation
peaks_graphic <- ggplot(rainLuq, aes(x = date, y = rainfall)) +
  geom_line(colour='blue') +
  labs(x = "Date",
       y = "Precipitation (mm, >62mm)") +
  geom_point(data=peaks,aes(x = as.POSIXct(row.names(peaks)), y = V1), colour='red')
peaks_graphic

#save graphic - revise file name for your dataset
ggsave("LUQ_precipitation_peaks.jpg", peaks_graphic,dpi=300)

#*** why are some of the seemingly 20 mm daily events not indicated as a peak (red dot)?
#*** peaks are defined relative to the preceding period of rainfall
#*** if preceding period was wet, a 20 mm or greater event will not be designated as a peak



# Step 4 ------------------------------------------------------------------
# Create a data frame to add to the group project
#name your LTER

#name your LTER
lter<-"Luq"

#name your site
site<-"ElVerde"

#name your driver
driver<-"Daily precipitation"
#units = units of driver (e.g., mm)
units<-"mm"

#are you analyzing peaks or valleys? [options: peak or valley]
pv="peak"

#save number of months and years
nmonths <- nmonths(Luqxts)
nyears <- nyears(Luqxts)



# Step 5 ------------------------------------------------------------------
# how many peaks per total obs, which are in units of days 
# (here, obs = 365 days*47 years) ?
peak_per_d <- length(peaks$V1)/length(rainLuq$rainfall)
peak_per_d

#how many peaks per year?
peaks_per_y <- length(peaks$V1)/nyears(Luqxts)
peaks_per_y

#average peak magnitude (in mm for precip)
peak_mean <- mean(peaks$V1)
peak_mean

#peak standard deviation
peak_sd <- sd(peaks$V1)

#peak CV
peak_CV <- sd(peaks$V1)/mean(peaks$V1)
peak_CV



# STEP 6: Standardized regression models for temporal change --------------

# peak number vs. time: Has the number of peaks increased/decreased over time?
# get slope of number of peaks per year for each year vs. year (and p-value)

# add year and time columns to peaks dataset
peaks$time <- as.POSIXct(row.names(peaks))
peaks$year <- as.numeric(format(as.POSIXct(row.names(peaks)),"%Y"))


# first, add any missing years that had no peaks (add zeros) - probably a more efficient way to do this...
year <- min(as.numeric(format(as.POSIXct(rainLuq$date),"%Y"))):max(as.numeric(format(as.POSIXct(rainLuq$date),"%Y")))
years<-as.data.frame(year)
years
peak.sum<-peaks %>% group_by(year) %>% summarise(mean.peak=mean(V1), count=n())
peak.sum
peak.number<-merge(years,peak.sum,by.x="year",by.y="year",all.x=TRUE)
peak.number[is.na(peak.number)] <- 0 
peak.number


# second, build the stats models and save the slope and p as output
peak.number.lm <- lm(count~year,data=peak.number)
#plot(peak.number.lm) # turn this on to check model statistical assumptions
lmsum.number <- summary(peak.number.lm)
peak.number.slope<-peak.number.lm$coefficients[2]
peak.number.slope
peak.number.p<-lmsum.number$coefficients[2,4]
peak.number.p



# peak magnitude (all peaks) vs. time
# get slope of magnitude of peaks vs. time (and p-value)
# note: this analysis does not include years that have no peaks (e.g., could set those to zero as in peak.number)
# but I think this is a better approach because it makes the analysis structure conditional, like a hurdle model
# step 1, has the number of peaks changed over time? If yes, then (conditional only on dates with peaks), has peak magnitude changed?
peak.magnitude.lm<-lm(V1~time,data=peaks)
#plot(peak.magnitude.lm) # turn this on to check model statistical assumptions
lmsum.mag<-summary(peak.magnitude.lm)
peak.magnitude.slope<-peak.magnitude.lm$coefficients[2]
peak.magnitude.slope
peak.magnitude.p<-lmsum.mag$coefficients[2,4]
peak.magnitude.p



#### Valleys example for show only *** doesn't work for ppt data ****
# if you have valleys pulses, do a find and replace in the code peak->valley and match case
#sev40valleys<-findValleys(sev40xts, thresh=0)
#plot(sev40xts[sev40valleys])
#sev40valleys<-findValleys(sev40xts, thresh=5)
#plot(sev40xts[sev40valleys])

######### Save metrics into data frame ##################
pulse_metrics_Luq<-data.frame(lter,site,driver,units,pv,nyears,nmonths,peak_mean,peak_sd,peak_CV,peaks_per_y,peak_per_d,
                                peak.number.slope,peak.number.p,peak.magnitude.slope,peak.magnitude.p)
write.csv(pulse_metrics_Luq,"LUQ_pulse_metrics.csv")
#then we will merge metrics data frame across LTER datasets using rbind



# METADATA for pulse quantmod metrics (long format)
#lter = 3 letter lter site acronym ALL CAPS
#site = local site code within lter site (e.g., sev40)
#driver = abiotic variable for pulse regime analysis
#units = units of driver (e.g., mm)
#[options: precipitation, temperature, waterdepth, flow, ... ]
#pv = are you analyzing valleys or valleys? [options: peak or valley]
#metric = name of pulse metric 
#[options: pulse_freq = number of pulse/number of observations
#value = numerical value of pulse metric
#nyears = number of years in the data set (units = years)
#nmonths = number of months in the data set (units = months)
#peak_mean = average magnitude of peak (units = units, defined above)
#peak_sd = standard deviation of the average magnitude of peak (units = units, defined above)
#peak_CV = coefficient of variation of the average magnitude of peak (peak_sd/peak_mean) (unitless)
#peaks_per_y = number of peaks per year averaged over time series (count data)
#peak_per_d = number of peaks per day averaged over time series (count data, fraction)
#peak.number.slope = slope of the number of peaks per year regressed on year (has the number of pulses increased/decreased over time?)
#peak.number.p = p-value for whether slope of the number of peaks per year regressed on year
#                is significantly different from zero, t-test from lm (has the number of pulses increased/decreased over time?)
#peak.magnitude.slope = the slope of the peak magnitude regressed on calendar date (has the magnitude of pulses increased/decreased over time?)
#peak.magnitude.p = p-value for whether the slope of the peak magnitude regressed on calendar date
#                   is significantly different from zero, t-test from lm (has the magnitude of pulses increased/decreased over time?)





# EXTRA -------------------------------------------------------------------

peaks <- ggplot(peak.number, aes(x = year, y = count)) + 
  geom_point() +
  xlab('Year')+ ylab("Number of peaks (>62mm/d)") +
  stat_smooth(method = "lm", col = "blue") + 
  
  stat_cor(label.y = 9,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_regline_equation(label.y = 8.5) +
  
  theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  theme_classic() 

peaks
peaks + ggsave("PeaksLuq100.jpeg",  width = 12, height = 10, units = "cm")                     


