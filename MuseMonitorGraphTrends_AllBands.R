# ---------------------------------------------------------------------------------------------------------------------- #

###### ------------------------- Code to calculate trends in Muse Monitor data -------------------------- ###### 
# ------------------- Holly Copeland ---------------------------------------------------------------------#
# ---------------- Contact: hecopeland@gmail.com---------------------------------------- ---------------- #
# Code entirely excludes files with nans, and removes any nodata from blinks, jaw clenches
# ---------------------------------------------------------------------------------------------------------------------- #

####  Load Required Packages ####
library(tidyverse)
library(data.table)
library(dplyr)

# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
#### CHANGE THIS LOCATION TO MATCH YOUR DATA ####
setwd("C:/Users/Holly Copeland/iCloudDrive/muse monitor/") 
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
#
dat$AF7Deltamean <- rowMeans(dat[c('Delta_AF7')], na.rm=TRUE)
dat$AF7Thetamean <- rowMeans(dat[c('Theta_AF7')], na.rm=TRUE)
dat$AF7Alphamean <- rowMeans(dat[c('Alpha_AF7')], na.rm=TRUE)
dat$AF7Gammamean <- rowMeans(dat[c('Gamma_AF7')], na.rm=TRUE)
dat$AF7Betamean <- rowMeans(dat[c('Beta_AF7')], na.rm=TRUE)
#
dat$AF8Deltamean <- rowMeans(dat[c('Delta_AF8')], na.rm=TRUE)
dat$AF8Thetamean <- rowMeans(dat[c('Theta_AF8')], na.rm=TRUE)
dat$AF8Alphamean <- rowMeans(dat[c('Alpha_AF8')], na.rm=TRUE)
dat$AF8Gammamean <- rowMeans(dat[c('Gamma_AF8')], na.rm=TRUE)
dat$AF8Betamean <- rowMeans(dat[c('Beta_AF8')], na.rm=TRUE)
#
dat$TP9Deltamean <- rowMeans(dat[c('Delta_TP9')], na.rm=TRUE)
dat$TP9Thetamean <- rowMeans(dat[c('Theta_TP9')], na.rm=TRUE)
dat$TP9Alphamean <- rowMeans(dat[c('Alpha_TP9')], na.rm=TRUE)
dat$TP9Gammamean <- rowMeans(dat[c('Gamma_TP9')], na.rm=TRUE)
dat$TP9Betamean <- rowMeans(dat[c('Beta_TP9')], na.rm=TRUE)
#
dat$TP10Deltamean <- rowMeans(dat[c('Delta_TP10')], na.rm=TRUE)
dat$TP10Thetamean <- rowMeans(dat[c('Theta_TP10')], na.rm=TRUE)
dat$TP10Alphamean <- rowMeans(dat[c('Alpha_TP10')], na.rm=TRUE)
dat$TP10Gammamean <- rowMeans(dat[c('Gamma_TP10')], na.rm=TRUE)
dat$TP10Betamean <- rowMeans(dat[c('Beta_TP10')], na.rm=TRUE)
#
dat$AlphaThetaRatio <- (dat$Alphamean / dat$Thetamean)
dat$ThetaBetaRatio <- (dat$Thetamean / dat$Betamean)
dat$GammaDeltaRatio <- (dat$Gammamean / dat$Deltamean)
#rescale from 0 to 100

#ReScale <- function(x,first,last){(last-first)/(max(x)-min(x))*(x-min(x))+first}
ReScale <- function(x,first,last){(last-first)/(1 - (-1))*(x - (-1))+first}
dat$DeltameanR <- ReScale (dat$Deltamean, 0, 100)
dat$ThetameanR <- ReScale (dat$Thetamean, 0, 100)
dat$AlphameanR <- ReScale (dat$Alphamean, 0, 100)
dat$GammameanR <- ReScale (dat$Gammamean, 0, 100)
dat$BetameanR <- ReScale (dat$Betamean, 0, 100)

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
  summarise(AF7deltamean= median(AF7Deltamean),AF7thetamean=median(AF7Thetamean),AF7alphamean = median(AF7Alphamean),AF7betamean=median(AF7Betamean),AF7gammamean = median(AF7Gammamean),
            AF8deltamean= median(AF8Deltamean),AF8thetamean=median(AF8Thetamean),AF8alphamean = median(AF8Alphamean),AF8betamean=median(AF8Betamean),AF8gammamean = median(AF8Gammamean),
            TP9deltamean= median(TP9Deltamean),TP9thetamean=median(TP9Thetamean),TP9alphamean = median(TP9Alphamean),TP9betamean=median(TP9Betamean),TP9gammamean = median(TP9Gammamean),
            TP10deltamean= median(TP10Deltamean),TP10thetamean=median(TP10Thetamean),TP10alphamean = median(TP10Alphamean),TP10betamean=median(TP10Betamean),TP10gammamean = median(TP10Gammamean),
            deltamean= median(Deltamean),thetamean=median(Thetamean),alphamean = median(Alphamean),betamean=median(Betamean),gammamean = median(Gammamean),
            thetabetaratiomean = median(ThetaBetaRatio), alphathetaratiomean = median(AlphaThetaRatio), gammadeltaratiomean = median(GammaDeltaRatio), na.rm=TRUE)

summary(datmeans)
datmeans$date <- as.Date(datmeans$date,"%m/%d/%Y", tz ="MST")

library (grid)
library (gridExtra)

#plots of distances and disturbance on noise
p1 <- ggplot(datmeans, aes(x=date, y=deltamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("delta")
p2 <- ggplot(datmeans, aes(x=date, y=thetamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("theta")
p3 <- ggplot(datmeans, aes(x=date, y=alphamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth() + ylab("alpha")
p4 <- ggplot(datmeans, aes(x=date, y=betamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("beta")
#p5 <- ggplot(datmeans, aes(x=date, y=gammamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b", breaks='1 month') + geom_smooth()+ ylab("gamma")
p6 <- ggplot(datmeans, aes(x=date, y=alphathetaratiomean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b", breaks='1 month') + geom_smooth()+ ylab("alpha/theta")
p7 <- ggplot(datmeans, aes(x=date, y=thetabetaratiomean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b", breaks='1 month') + geom_smooth()+ ylab("theta/beta")
p8 <- ggplot(datmeans, aes(x=date, y=gammadeltaratiomean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b", breaks='1 month') + geom_smooth()+ ylab("gamma/delta")

p9 <- ggplot(datmeans, aes(x=date, y=AF7deltamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("AF7 delta")
p10 <- ggplot(datmeans, aes(x=date, y=AF7thetamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("AF7 theta")
p11 <- ggplot(datmeans, aes(x=date, y=AF7alphamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth() + ylab("AF7 alpha")
p12 <- ggplot(datmeans, aes(x=date, y=AF7betamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("AF7 beta")
p13 <- ggplot(datmeans, aes(x=date, y=AF7gammamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b", breaks='1 month') + geom_smooth()+ ylab("AF7 gamma")

p14 <- ggplot(datmeans, aes(x=date, y=AF8deltamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("AF8 delta")
p15 <- ggplot(datmeans, aes(x=date, y=AF8thetamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("AF8 theta")
p16 <- ggplot(datmeans, aes(x=date, y=AF8alphamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth() + ylab("AF8 alpha")
p17 <- ggplot(datmeans, aes(x=date, y=AF8betamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("AF8 beta")
p18 <- ggplot(datmeans, aes(x=date, y=AF8gammamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b", breaks='1 month') + geom_smooth()+ ylab("AF8 gamma")

p19 <- ggplot(datmeans, aes(x=date, y=TP9deltamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("TP9 delta")
p20 <- ggplot(datmeans, aes(x=date, y=TP9thetamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("TP9 theta")
p21 <- ggplot(datmeans, aes(x=date, y=TP9alphamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth() + ylab("TP9 alpha")
p22 <- ggplot(datmeans, aes(x=date, y=TP9betamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("TP9 beta")
p23 <- ggplot(datmeans, aes(x=date, y=TP9gammamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b", breaks='1 month') + geom_smooth()+ ylab("TP9 gamma")

p24 <- ggplot(datmeans, aes(x=date, y=TP10deltamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("TP10 delta")
p25 <- ggplot(datmeans, aes(x=date, y=TP10thetamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("TP10 theta")
p26 <- ggplot(datmeans, aes(x=date, y=TP10alphamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("TP10 alpha")
p27 <- ggplot(datmeans, aes(x=date, y=TP10betamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("TP10 beta")
p28 <- ggplot(datmeans, aes(x=date, y=TP10gammamean)) + geom_line(alpha = 0.2) + scale_x_date(date_labels = "%b",breaks='1 month') + geom_smooth()+ ylab("TP10 gamma")

#grid.arrange(p2, p6, p7, p10, p15, p20, p25, ncol=4, nrow = 2)

grid.arrange(p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28, ncol=5,nrow=4)

dim(dat) # nrows,
#grep('NaN', dat) #to check if the dat object contains any NaN
