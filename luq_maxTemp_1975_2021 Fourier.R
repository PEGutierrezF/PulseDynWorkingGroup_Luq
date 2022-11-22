



# ---------------------------------------------
# Max temperature El Verde Field Station 1975-2021
# Pulse metrics using Fourier time-series analyses
# 12 Sep 2022
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

