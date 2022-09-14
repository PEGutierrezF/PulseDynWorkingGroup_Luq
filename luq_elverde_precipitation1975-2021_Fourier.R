



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
All_var_1975_2021=read.csv("All_variables 1975-2021.csv")
head(All_var_1975_2021)
summary(All_var_1975_2021)

maxTempLuq <- All_var_1975_2021 %>% select(date, max_temp, min_temp) %>% # select date, max and min
  mutate(max_temp_apro = na.approx(max_temp)) %>% # extrapolate max
  mutate(min_temp_apro = na.approx(min_temp)) # extrapolate min
summary(maxTempLuq)



# Select only Maximum Temperature -----------------------------------------
maxTempLuq_apro <- maxTempLuq %>% select(date, max_temp_apro) # let's work with max first
summary(maxTempLuq_apro)


#positivize the data for max_temp_apro by adding 20, cannot take log of zero/negative
maxTempLuq_apro$max_temp_apro <- maxTempLuq_apro$max_temp_apro+20


### date format ###
maxTempLuq_apro$date <- as.Date(maxTempLuq_apro$date)

### Fourier Analysis & Data Extremes ###
#Create 'streamflow' object
luq_Tmax.data<-asStreamflow(maxTempLuq_apro,start.date="1975-01-01",
                             end.date="2021-12-31", river.name="El Verde")

###Plot data to visually inspect time-series pulses
luq_Tmax.data<-luq_Tmax.data$data
plot(luq_Tmax.data$discharge ~ luq_Tmax.data$date, type="l", ylab="Maximum Air Temperature (C)", xlab="Date")






