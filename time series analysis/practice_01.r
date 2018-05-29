#this is for my own learning purpose.
library(TSA);library(lubridate);library(stringr)
library(sqldf);library(xts);library(dplyr)
library(forecast)

#outlier detection
data01 <- read.csv(paste("H:/01_self study_flash drive/AAAschulich",
                        "/aaa_ma/time series/R learning/r_reference/R_exercise/ts_try/",
                        "tele_case/call_data_original.csv",sep=""))


#select date, call offered, avg handle time column.
data02 <- data01[,c(1,2,5)]

#check col type
str(data02)
summary(data02)

names(data02) <-c("date","call_offer","avg_time")

#change factor to char or numeric
data02$date <- as.character(data02$date)
data02$avg_time <-as.numeric(as.character(data02$avg_time))


#histogram
hist(data02$call_offer)
hist(data02$avg_time)

#add month and year 
date02 <-as.Date(data02$date,format="%m/%d/%Y")
month <-  format(date02,"%m")
year <- format(date02,"%Y")
day_of_week <- str_sub(weekdays(date02),1,2)
data02$month <- month; data02$year <- year; data02$weekday <- day_of_week

#boxplot
par(mfrow=c(1,1))
boxplot(data02$call_offer~data02$weekday,data=data02,
        main="call offered across week")

boxplot(data02$call_offer~data02$month,data=data02,
        main="call offered across month",xlab="Month")
boxplot(data02$avg_time~data02$weekday,data=data02,
        main="avg handle time across week")

#check small value
data02[data02$call_offer<2000,]
outlier_call_offer <- which(data02$call_offer<1000)
data02[data02$avg_time<500,]

#imputation june call_offer and avg_time
med_call_offer <- median((subset(data02,month=="06"))$call_offer,na.rm=T)
med_avg_time <- median((subset(data02,month=="06"))$avg_time,na.rm=T)

data02$call_offer[outlier_call_offer]<- med_call_offer
data02$avg_time[outlier_call_offer] <- med_avg_time
data02$avg_time[1224] <- med_avg_time

#check data02
summary(data02)

subset(data02,is.na(data02$avg_time==T))
subset(data02,is.na(data02$call_offer==T))

#check row and col
dim(data02)

#change data to ts
x1 <- ts(data02$call_offer,start=c(2015,1),freq=365)
x2 <- ts(data02$avg_time,start=c(2015,1),freq=365)

#summarize to month day
data02_df <- tbl_df(data02)

data03_df <- data02_df %>%
  group_by(month,year) %>%
  summarize(call_o_sum=sum(call_offer),
   handle_time_avg=mean(avg_time)) %>%
  arrange(year,month)

#transform may call offered
data03_df$call_o_sum[41] <- (data03_df$call_o_sum)[41] /16*31
  
tail(data03_df)

#transform data to ts
ts_call_month <- ts(data03_df$call_o_sum, start=c(2015,1),freq=12)


#check monthly data distribution
hist(ts_call_month, main="Histogram of monthly call offered")

#not normal, power transform
BoxCox.ar(ts_call_month,lamda=seq(0,20,0.1),method="mle")
ts_call_month <- (ts_call_month)^2.1

#check distribution again
hist(ts_call_month,main="Histogram of monthly call offered after transform")

#plot
plot(ts_call_month,type="o",main="Monthly call offered after transformation")

#check stationarity
adf.test(ts_call_month)
#which shows not staionary, need diff.

adf.test(diff(ts_call_month))
#still not stationary, need diff again

adf.test(diff(diff(ts_call_month)))
#now it is stationary

#plot differenced data
plot(diff(diff(ts_call_month)),type="o",main="No. of transformed onthly call after two difference",
     ylab="")

#plot acf and pacf
#change the lag by multiply by 12 to change from year to month!
#other wise the x axis will show decimal lag because it is year based.
i <- diff(diff(ts_call_month))
acf_change <- acf(i,plot=F)
acf_change$lag <- acf_change$lag *12

plot(acf_change,main="ACF for monthly call")

pacf_change <- pacf(i, plot=F)
pacf_change$lag <- pacf_change$lag*12

plot(pacf_change,main="PACF for monthly call")

#use auto.arima to select model
auto.arima(ts_call_month,d=2,D=NA,max.p=14,max.q=14,max.P=14,max.Q=14)

model01 <- arima(ts_call_month, order=c(1,2,1),seasonal=list(
                 order=c(0,0,1),period=3))


#close dplyr library so there is no confusion in tsdiag 
detach(package:dplyr, unload=TRUE)

#model diagnose 
tsdiag(model01)
par(mfrow = c(1,1))
#check normality of residuals
hist(rstandard(model01),xlab = "Standardized Residuals",
     main="Histogram of residuals")
qqnorm(rstandard(model01))
qqline(rstandard(model01))

shapiro.test( rstandard( model01 ) )
#forecasting
par(mfrow = c(1,1))

result <- plot(model01,n1=c(2015,1),n.ahead=24,pch=19,xlab="time: year",
  ylab="transformed data",main="monthly calls offered forecast to 2020"
               ,col="blue")

#put forecasting in original units
head(result$pred)

#save pred, upi, lpi
model01_pred <- (result$pred)^(1/2.1)

model01_upi <- (result$upi)^(1/2.1)

model01_lpi <- (sign(result$lpi)*(abs(result$lpi))^(1/2.1))

head(model01_pred)


model01_pred <- ts(model01_pred,start=c(2018,6),freq=12)


plot((ts_call_month)^(1/2.1),type="o",xlim=c(2016.4,2020.5),
     ylim=c(min(model01_lpi*1.1),max(model01_upi)*1.1),
     xlab="time: year", ylab="total no. of monthly call",
     main="Forecast total monthly call of the next two years")
points(model01_pred,type="o",lty=2,col="black",pch=19)
points(model01_upi,type="l",col="blue",lty=3)  
points(model01_lpi,type="l",col="blue",lty=3)



#number of call daily
ts_call_day <- ts( (data02[1097:1232,])$call_offer,start=c(2018,1),frequency=365)
plot(ts_call_day,type="o")


#check normal distribution
hist(ts_call_day,main="Histogram before transformation")
#not normal

#transformation
BoxCox.ar(ts_call_day,lamda=sep(0,20,0.1),method="mle")

ts_call_day <- (ts_call_day)^(-1/9)
hist(ts_call_day,main="Histogram after transformation")

#check stationarity of transformed daily call
set.seed(1234)
adf.test(ts_call_day)
#it is stationary!

plot(ts_call_day,type="o",main="Monthly call offered in 2018")


acf_chang2 <- acf(ts_call_day,lag.max=40)
acf_chang2$lag <- acf_chang2$lag *365

pacf_change2 <- pacf(ts_call_day,lag.max=40)
pacf_change2$lag <- pacf_change2$lag *365

plot(acf_chang2,main="ACF for daily call")
#we can see seasonality in day 7, 14, 21, 28, ...
plot(pacf_change2,main="PACF for daily call")

#use auto.arima to select model
auto.arima(ts_call_day,d=NA,D=NA,max.p=15,max.q=15,max.P=15,max.Q=15)

model02 <- arima(ts_call_day, order=c(1,0,0),seasonal=list(
           order=c(1,0,1),period=7))

model02

#model diagnose
tsdiag(model02)

#check normality of residuals
par(mfrow = c(1,1))
hist(rstandard(model02),xlab = "Standardized Residuals",
     main="Histogram of residuals")
qqnorm(rstandard(model02))
qqline(rstandard(model02))

shapiro.test(rstandard(model02))
#forecasting


result2 <- plot(model02,n1=c(2018,1),n.ahead=60,pch=19,xlab="time: month",
               ylab="transformed data",main="Daily calls offered forecast to 60 days"
               ,col="blue")



#put forecasting in original units
head(result2$pred)

#save pred, upi, lpi
model02_pred <- (result2$pred)^(-9)

model02_upi <- (result2$upi)^(-9)

model02_lpi <- (result2$lpi)^(-9)

head(model02_pred)
ts_call_day <- (ts_call_day)^(-9)

#note: the x-axis is year, so 2018.5 means june, 2018
plot(ts_call_day,type="o",xlim=c(2017.99,2018.56),
     ylim=c(min(ts_call_day*0.8),max(ts_call_day)*1.05),
     xlab="time: year", ylab="total no. of daily call",
     main="Forecast total daily call of the next two months")
points(model02_pred,type="o",lty=2,col="black",pch=20,lwd=0.6)
points(model02_upi,type="l",col="blue",lty=3)  
points(model02_lpi,type="l",col="blue",lty=3)



































       