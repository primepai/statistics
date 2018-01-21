#adf functions test
#	ADF only tests for unit root stationary, this could be trend stationary. 
#So you should use the KPSS test...
#Trend stationary means "stationary around the trend", 
#i.e. the trend needn't be stationary, but the de-trended data is. 
#Level stationary means that the data is like white noise.
#see reference COMPARING ADF TEST FUNCTIONS IN R by Fabian Kostadinov!
#in sum, if you want to check trend stationary use adf.test(), if you want to
#check level stationary use adfTest(); seems conflict with wang yan's book of p218??

#to test different time series using adf functions
#1. pure randome walk, y(t)=y(t-1)+e(t) e(t) is white noise iid. Note this series is
# difference-stationary (DS)
library(tseries);library(fUnitRoots)

n <- 200
s1 <- rep(0,n)
for (i in 1:(n-1)){
  s1[i+1] <- s1[i]+rnorm(1,0,1)
}
plot(x=c(1:n),y=s1,type="o")
#we know that s1 is unstable (this is an AR(1) series with phi=1). 
#How do we test it? well, we can see its autocorrelation plot or we can do the adf test
acf(s1)
adf.test(s1)
adfTest(s1,lags=0,type="nc")

#2. random walk with a drift, y(t)=a+y(t-1)+e(t); a is a constant. Note this series
# is also difference-stationary (DS)
library(tseries);
n <- 200
s2 <- rep(0,n)
for (i in 1:(n-1)){
  s2[i+1] <- 0.8+s2[i]+rnorm(1,0,1)
}
plot(x=c(1:n),y=s2,type="o")
acf(s2)
adf.test(s2)
adfTest(s2,lags=0,type="c")

#3. deterministic trend, y(t)=a+b*t+e(t); a, b are constants. 
library(tseries);
n <- 200
s3 <- rep(0,n)
for (i in 1:(n-1)){
  s3[i+1] <- 0.8+1.5*(i+1)+rnorm(1,0,1)
}
plot(x=c(1:n),y=s3,type="o")
acf(s3)
adf.test(s3)
adfTest(s2,lags=0,type="ct")
