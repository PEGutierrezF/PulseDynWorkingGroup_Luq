




install.packages("aplpack")
library(aplpack)

setwd("D:/Curriculum/14_ Colaboracion/2021 Pulse LTER/Pulse WG PR/PulseDynWorkingGroup_Luq/data")
bagplot_Pool0=read.csv("bagplot.csv")
head(bagplot_Pool0)
summary(bagplot_Pool0)

bagplot_Pool0$date <-as.POSIXct(bagplot_Pool0$date,"%Y-%m-%d",tz = "UTC")

bagplot(bagplot_Pool0,factor=2.5,create.plot=TRUE,approx.limit=300,
        show.outlier=TRUE,show.looppoints=TRUE,
        show.bagpoints=TRUE,dkmethod=2,
        show.whiskers=TRUE,show.loophull=TRUE,
        show.baghull=TRUE,verbose=FALSE)

attach(bagplot_Pool0)
bagplot(bagplot_Pool0$date,bagplot_Pool0$daily_discharge_max,verbose=FALSE,dkmethod=2)





# Outliers ----------------------------------------------------------------
# https://rdrr.io/cran/GmAMisc/man/outlier.html

# n observation is considered outlier if the absolute difference between that 
# observation and the sample mean is more than 2 Standard Deviations away 
# (in either direction) from the mean.

# In the plot, the central reference line is indicating the mean value, 
# while the other two are set at mean-2*SD and mean+2*SD.

outlier(bagplot_Pool0$daily_discharge_max, method="mean", addthres=TRUE)

data=read.csv("graph.csv")
data$date <-as.POSIXct(data$date,"%Y-%m-%d",tz = "UTC")

ggplot(data,aes(date, daily_discharge_max)) +
  geom_line( size=0.5)+
  theme(legend.position = "none")
