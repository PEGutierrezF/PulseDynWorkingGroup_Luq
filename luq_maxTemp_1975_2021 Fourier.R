



# ---------------------------------------------
# Max temperature El Verde Field Station 1975-2021
# Pulse metrics using Fourier time-series analyses
# Nov 22 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  


rm(list=ls(all=TRUE)) #give R a blank slate




All_var_1975_2021=read.csv("All_variables 1975-2021.csv")

maxTempLuq <- All_var_1975_2021 %>% select(date, max_temp, min_temp) %>% # select date, max and min
  mutate(max_temp_apro = na.approx(max_temp)) %>% # extrapolate max
  mutate(min_temp_apro = na.approx(min_temp)) # extrapolate min
summary(maxTempLuq)

# Select only Maximum Temperature -----------------------------------------
Luq_temp <- maxTempLuq %>% select(date, max_temp_apro) # let's work with max first
summary(Luq_temp)


# Step 1 ------------------------------------------------------------------

#positivize the data for rainfall by adding 20, cannot take log of zero/negative
Luq_temp$max_temp_apro <- Luq_temp$max_temp_apro


# Step 2 ------------------------------------------------------------------

Luq_temp$date <-as.POSIXct(Luq_temp$date,"%Y-%m-%d",tz = "UTC")
summary(Luq_temp)


# Step 3 ------------------------------------------------------------------

### Fourier Analysis & Data Extremes ###
#Create 'streamflow' object
Luq_temp <- asStreamflow(Luq_temp, start.date="1975-01-01",
                        end.date="2021-03-30", 
                        river.name="El Verde")

Luq_temp



# Step 4 ------------------------------------------------------------------

###Plot data to visually inspect time-series pulses
Luq_temp <- Luq_temp$data
plot(Luq_temp$discharge~Luq_temp$date, type="l", 
     ylab="Max Temperature (C)", xlab="Date")



# Step 5 ------------------------------------------------------------------
# I think there is an error. 
# Star in Step 3 and skip step 4

#Fourier Analysis
Luq_temp.fourier <- fourierAnalysis(Luq_temp)
Luq_temp.fourier
summary(Luq_temp.fourier) # SUMMARY STATS
#1)"noise color" and is a measure of flashiness. Noise color ranges from 0 (white noise with no autocorrelation-highly flashy) to values
#over 2 (reddened, values with strong autocorrelation in which high- or low- events persist over the short term).
#2)"signal-noise ratio" is a measure of seasonality (higher is more seasonal).
#3) "average" is long-term ordinal day average.
plot(Luq_temp.fourier, plot.type="hydrograph") # PLOT



# Step 6 ------------------------------------------------------------------

#Calculate annual extremes
Luq_temp.extremes <- annualExtremes(Luq_temp)
names(Luq_temp.extremes)


# Step 7. Export results --------------------------------------------------

# allstats function works on files
# write R 'streamflow' object data into csv file
Luq_temp <- Luq_temp$data
write.csv(Luq_temp, "Luq_temp_Fourier_Nov-22-2022.csv")

Luq_temp.allstats <- allstats(file.name = "All_variables 1975-2021.csv", river.name = "El Verde", 
                             file.type="csv", date.col=1,
                             discharge.col=4, skipped.rows=0)
Luq_temp.allstats

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
# This Section Use this:  Luq_temp <- Luq_temp$data Step 4
median.year <- tapply(Luq_temp$discharge, list(Luq_temp$year), median)
median.sum <- median(median.year) #40.415
median.jday <- tapply(Luq_temp$discharge, list(Luq_temp$jday), median)
jday <- tapply(Luq_temp$jday, list(Luq_temp$jday), mean)
year <- tapply(Luq_temp$year, list(Luq_temp$year), mean)

### Long-term min and max per year ###
max.annual <- tapply(Luq_temp$discharge, list(Luq_temp$year), max)
min.annual <- tapply(Luq_temp$discharge, list(Luq_temp$year), min)


###Phenologies to assess mean date and CV
###min<- lm(jday~year) ### slope and p-value could indicate pheno-shifts (pulse metric)
###max<- lm(jday~year) ### slope and p-value could indicate pheno-shifts (pulse metric)
###days.min and days.max could assess the pulse phenology and character
###max.jday$annual.max - min.jday$annual.min
min <- lm(min.annual~year)
summary(min)
min.slope<- -0.004972
min.p <- 0.667
plot(min)

#Quantile regression
min.annual<-as.vector(min.annual)
year<-as.vector(year)
min.rq<-rq(min.annual~year)
summary(min.rq)
min.rq.slope<-0.0000

#plot(min) #use to check residuals
plot(min.annual~year)



max<-lm(max.annual~year)
summary(max)
max.slope <- 0.04939
max.p <- 0.0212

#Quantile regression
max.annual<-as.vector(max.annual)
year<-as.vector(year)
max.rq<-rq(max.annual~year)
summary(max.rq)
max.rq.slope<-0.04857

#plot(max) #use to check residuals
plot(max.annual~year)




# Step 8 ------------------------------------------------------------------
# Luq_temp.extremes <- annualExtremes(Luq_temp) from Step 6

f1<-data.frame(Luq_temp.extremes$annual.max)
max.window<-(max(f1$jday)-min(f1$jday))
###Maximum period in-between peak Tmax period 335 days###

f2<-data.frame(Luq_temp.extremes$annual.min)
min.window<-(max(f2$jday)-min(f2$jday))
###Maximum period in-between low Tmax period 365 days###


### Plot annual.min and annual.max values ###
par(mfrow=c(1,2), mar = c(5, 5, 1, 1))
plot(f1$discharge~f1$jday, ylab="Maximun Temperature (C)", xlab="Julian Day", ylim=c(0,60), xlim=c(0,360))
par(new=TRUE)
plot(jday, median.jday, type="l", ylim=c(0,60), xlim=c(0,360), ylab="", xlab="")
abline(40.415,0)

plot(f2$discharge~f2$jday, ylab="Maximun Temperature (C)", xlab="Julian Day", ylim=c(0,60), xlim=c(0,360))
par(new=TRUE)
plot(jday, median.jday, type="l", ylim=c(0,60), xlim=c(0,360), ylab="", xlab="")
abline(40.415,0)



####################################3
#name your LTER
lter<-"Luq"

#name your site
site<-"El Verde"

#name your driver
driver<-"Maximun temperature"
#units = units of driver (e.g., mm)
units<-"C"

#maximum window
max <- max.window
max.units<-"days"

#minimum window
min <- min.window
min.units<-"days"

#maximum slope
max.slope<-max.slope

#maximum slope P-value
max.p<-max.p

#miniimum slope
min.slope<-min.slope

#minimum slope P-value
min.p<-min.p

######### Save metrics into data frame ##################
pulse_metrics_rainLqu <- data.frame(lter,site,driver,units,max,max.units,
                                    min,min.units,max.slope,max.p,min.slope,
                                    min.p, RainLuq.allstats)

write.csv(pulse_metrics_rainLqu,"Luq_maxTempature_Fourier_Nov-22-2022.csv")
#then we will merge metrics data frame across LTER datasets using rbind
