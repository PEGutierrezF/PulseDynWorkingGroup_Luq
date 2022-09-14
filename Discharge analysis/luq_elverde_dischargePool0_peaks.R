



# ---------------------------------------------
# Discharge Pool 0. El Verde Field Station
# Peaks and Valleys with "quantmod" packages
# 15 Aug 2022
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  


# cleans global environment
rm(list = ls())



setwd("D:/Curriculum/14_ Colaboracion/2021 Pulse LTER/Pulse WG PR/PulseDynWorkingGroup_Luq/data")

pool0 = read.csv("disc_Pool0_mean.csv", header=TRUE)[,1:2]
summary(pool0)

### ISO date format ###
pool0$date<-as.POSIXct(pool0$date,"%Y-%m-%d",tz = "UTC") 
summary(pool0)


Luq_pool0_xts <- xts(pool0[,-1], order.by=pool0[,1])

# Define threshold as peaks distributed >98%. So, 7.413486
quantile(Luq_pool0_xts,0.98)


length(which(pool0$Prieta_pool0 > 7.413486))
length(pool0$Prieta_pool0 [pool0$Prieta_pool0  >7.413486])

hist(pool0$Prieta_pool0 )


# Step 2 ------------------------------------------------------------------
# set a threshold here:
threshold.peak <- 7.413486

# Step 3 ------------------------------------------------------------------
# Function to identify peaks[valleys]
LUQ_pool0_peaks<-findPeaks(Luq_pool0_xts, thresh=threshold.peak)
plot(Luq_pool0_xts[LUQ_pool0_peaks-1])


# GRAPH PEAKS
#turn peaks into a dataframe to add it to a ggplot of the raw data
#and calculate metrics
#here we use 62 threshold
peaks <- as.data.frame(Luq_pool0_xts[LUQ_pool0_peaks-1])
head(peaks)


#plot peaks onto raw data for precipitation
peaks_graphic <- ggplot(pool0, aes(x = date, y = Prieta_pool0)) +
  geom_line(colour='blue') +
  labs(x = "Date",
       y = "Discharge (m3/s, >7)") +
  geom_point(data=peaks,aes(x = as.POSIXct(row.names(peaks)), y = V1), colour='red')
peaks_graphic
