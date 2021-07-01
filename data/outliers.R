




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

boxplot(bagplot_Pool0$daily_discharge_max)




# Outliers ----------------------------------------------------------------

outlier(bagplot_Pool0$daily_discharge_max, method="median", addthres=TRUE)


# Example
# create a toy dataset
mydata <- c(2,3,4,5,6,7,8,9,50,50)

# locate outlier(s) using the median-based method
outlier(mydata, method="median", addthres=TRUE)
