# --------------------------------------------------------------------------------------------------------------#

###### ------------------------- Code to calculate trends in Muse Monitor data -------------------------- ####### 
# ------------------- Holly Copeland ---------------------------------------------------------------------------#
# ---------------- Contact: holly@heartmindalchemy.com---------------------------------------- ---------------- #
# Code entirely excludes files with nans, and removes any nodata from blinks, jaw clenches
# --------------------------------------------------------------------------------------------------------------#

####  Load Required Packages ####
library(tidyverse)
library(data.table)
library(dplyr)

# --------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------
#### CHANGE THIS LOCATION TO MATCH YOUR DATA ####

setwd("C:/Users/Holly Copeland/iCloudDrive/muse monitor") 
 
###################################################
library(stringr)
listcsv <- dir(pattern = "*.csv")
dat <- data.frame()
#Loop through files and do not include any with NaNs
for (i in 1:length(listcsv)){
  if(length(grep('NaN', read.csv(listcsv[i])))>0){
    next
  }
  dat <- rbind(dat, read.csv(listcsv[i]))
}

dat$Deltamean <- rowMeans(dat[c('Delta_AF7', 'Delta_TP9', 'Delta_AF8','Delta_TP10')], na.rm=TRUE)
dat$Thetamean <- rowMeans(dat[c('Theta_AF7', 'Theta_TP9', 'Theta_AF8','Theta_TP10')], na.rm=TRUE)
dat$Alphamean <- rowMeans(dat[c('Alpha_AF7', 'Alpha_TP9', 'Alpha_AF8','Alpha_TP10')], na.rm=TRUE)
dat$Gammamean <- rowMeans(dat[c('Gamma_AF7', 'Gamma_TP9', 'Gamma_AF8','Gamma_TP10')], na.rm=TRUE)
dat$Betamean <- rowMeans(dat[c('Beta_AF7', 'Beta_TP9', 'Beta_AF8','Beta_TP10')], na.rm=TRUE)
dat$AlphaThetaRatio <- (dat$Alphamean / dat$Thetamean)
dat$ThetaBetaRatio <- (dat$Thetamean / dat$Betamean)
dat$GammaDeltaRatio <- (dat$Gammamean / dat$Deltamean)

dat$datetime <- as.POSIXct(strptime(dat$TimeStamp,format = "%Y-%m-%d %H:%M:%S"), tz ="MST")
dat$date <- format(as.POSIXct(dat$datetime,format='%Y-%m-%d %H:%M:%S'),format='%m/%d/%Y')
dat <- dat[complete.cases(dat[ , 2:21]),]
##Go through each row and determine if a value is zero 
row_sub = apply(dat, 1, function(row) all(row !=0 ))
##Subset as usual
dat[row_sub,]
dat[order(as.Date(dat$datetime, format="%Y-%m-%d %H:%M:%S")),]
head(dat)
datmeans <- dat %>% group_by(date) %>% 
  summarise(deltamean= median(Deltamean),thetamean=median(Thetamean),alphamean = median(Alphamean),betamean=median(Betamean),gammamean = median(Gammamean), thetabetaratiomean = median(ThetaBetaRatio), alphathetaratiomean = median(AlphaThetaRatio), gammadeltaratiomean = median(GammaDeltaRatio), na.rm=TRUE)

summary(datmeans)
datmeans$date <- as.Date(datmeans$date,"%m/%d/%Y", tz ="MST")

library (grid)
library (gridExtra)

#plots of distances and disturbance on noise
p1 <- ggplot(datmeans, aes(x=date, y=deltamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("delta")
p2 <- ggplot(datmeans, aes(x=date, y=thetamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("theta")
p3 <- ggplot(datmeans, aes(x=date, y=alphamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth() + ylab("alpha")
p4 <- ggplot(datmeans, aes(x=date, y=betamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("beta")
p5 <- ggplot(datmeans, aes(x=date, y=gammamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b", breaks='1 month') + geom_smooth()+ ylab("gamma")
p6 <- ggplot(datmeans, aes(x=date, y=alphathetaratiomean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b", breaks='1 month') + geom_smooth()+ ylab("alpha/theta")
p7 <- ggplot(datmeans, aes(x=date, y=thetabetaratiomean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b", breaks='1 month') + geom_smooth()+ ylab("theta/beta")
p8 <- ggplot(datmeans, aes(x=date, y=gammadeltaratiomean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b", breaks='1 month') + geom_smooth()+ ylab("gamma/delta")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=4, nrow = 2)

dim(dat)
