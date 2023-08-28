data1=read.csv("C:/Users/DELL/OneDrive/Desktop/Final project/project data(monthly).csv")

data1
x = ts(data1[,2],frequency = 12)  # Draw 1st time series
y = ts(data1[,3],frequency = 12)    # Draw 2nd time series
z = ts(data1[,4],frequency = 12)  # Draw 3rd time series

s1= apply(cbind(x,y,z),2,min)
s2= apply(cbind(x,y,z),2,max)
plot(x,ylim=c(min(s1),max(s2)),main="combi")    # plot x and change the y axis limit
lines(y,type="l",col=2,)            # add y ts in x
lines(z,type="l",col=3)            # add z ts in x
legend("bottomright",c("USD","GBP","EURO"),col=1:3,lty=1)
install.packages("tseries")
library(tseries)
plot(decompose(x)) # for checking seasonality,trend,random
decompose(x)
plot(decompose(y))  # ......
decompose(y)
plot(decompose(z))  #......
decompose(z)

## stationary check for parametric of Mathematical Investigation
adf.test(x,alternative = c("stationary","explosive"),k=1) #k=lag
adf.test(y,alternative = c("stationary","explosive"),k=1) #k=lag
adf.test(z,alternative = c("stationary","explosive"),k=1) #k=lag
 
## stationary check for Diagammatic Investigation 
# 1st order difference
plot(x)
Box.test(x,lag = 1,type = "Ljung-Box")
a=diff(x)
plot(a)
adf.test(a,alternative = c("stationary","explosive"),k=1)

#Ljung Box test(for time series does contain autocorrelation)
Box.test(a,lag=1,type="Ljung-Box")


plot(y)
Box.test(y,lag = 1,type = "Ljung-Box")
b=diff(y)
plot(b)
adf.test(b,alternative = c("stationary","explosive"),k=1)
Box.test(b,lag=1,type="Ljung-Box")

plot(z)
Box.test(z,lag=1,type="Ljung-Box")
c=diff(z)
plot(c)
adf.test(c,alternative = c("stationary","explosive"),k=1)
Box.test(c,lag=1,type="Ljung-Box")

# For ACF 
acf(a)
acf(b)
acf(c)

#for pacf
pacf(a)
pacf(b)
pacf(c)



fit_exp = HoltWinters(x,gamma = TRUE) # 
fit_exp
fit_exp1 = HoltWinters(y,gamma = TRUE)
fit_exp1
fit_exp2 = HoltWinters(z,gamma = TRUE)
fit_exp2
Compare=matrix(c(0.65683910,-0.2170216,0.31110990,0.08852022,-0.5490243,-0.05611395,-0.31360945,-1.7152104,0.07038838,-0.081202242,-3.7662994,-1.53020513,-0.72494776,-1.9273746,-0.49409933,-0.03516756,-1.7348712,-0.67613691,0.43086455,-0.3418898,0.02939084,-0.12896226,0.5582742,0.92308523,0.29317180,0.8265519,1.32554453,0.04447505,0.5110292,1.02544097,0.18224252,0.7986710,0.94354485,0.65250910,-0.2595976,-0.06097890),nrow = 3,ncol = 12)
rownames(Compare)=c("Rupees vs USD","Rupees vs GBP","Rupees vs EURO")
colnames(Compare)=c("Jan","Feb","Mer","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
A=as.table(Compare)
A

## sarima fit for c(1,1,1)
sarima_1=arima(x,order=c(1,1,1),seasonal =list(order=c(1,1,1),period=1))
sarima_1
resid_1=sarima_1$residuals
resid_1
library(forecast)
forecast(sarima_1,h=12)

#normality check
jarque.bera.test(resid_1)


sarima_2=arima(y,order=c(1,1,1),seasonal =list(order=c(1,1,1),period=1))
sarima_2
resid_2=sarima_2$residuals
resid_2
forecast(sarima_2,h=12)

jarque.bera.test(resid_2)


sarima_3=arima(z,order=c(1,1,1),seasonal = list(order=c(1,1,1),period=1))
sarima_3
resid_3=sarima_3$residuals
resid_3
forecast(sarima_3,h=12)

jarque.bera.test(resid_3)

#calculate SSE for c(1,1,1)
SSE_1=sum(sarima_1$residuals^2)
SSE_1
SSE_2=sum(sarima_2$residuals^2)
SSE_2
SSE_3=sum(sarima_3$residuals^2)
SSE_3

##sarima fit for c(1,1,2)
sarima_4=arima(x,order = c(1,1,2),seasonal = list(order=c(1,1,2),period=1))
sarima_4
resid_4=sarima_4$residuals
resid_4
forecast(sarima_4,h=12)
jarque.bera.test(resid_4)

##For transformation
install.packages("MASS")
install.packages("forecast")
library(MASS)
library(forecast)
lambda=BoxCox.lambda(resid_4)
lambda
y_star=(resid_4^(lambda)-1)/lambda
y_star

sarima_5=arima(y,order = c(1,1,2),seasonal = list(order=c(1,1,2),period=1))
sarima_5
resid_5=sarima_5$residuals
resid_5

forecast(sarima_5,h=12)
jarque.bera.test(resid_5)
var_1=var(resid_5)
var_1
sqrt_1=sqrt(var_1)
sqrt_1
std_resid=resid_5/sqrt_1
std_resid

sarima_6=arima(z,order = c(1,1,2),seasonal = list(order=c(1,1,2),period=1))
sarima_6
resid_6=sarima_6$residuals
resid_6
forecast(sarima_6,h=12)
jarque.bera.test(resid_6)

# calculate SSE for c(1,1,2)
SSE_4=sum(sarima_4$residuals^2)
SSE_4
SSE_5=sum(sarima_5$residuals^2)
SSE_5
SSE_6=sum(sarima_6$residuals^2)
SSE_6

##sarima fit for c(2,1,1)
sarima_7=arima(x,order = c(2,1,1),seasonal = list(order=c(2,1,1),period=1))
sarima_7
resid_7=sarima_7$residuals
resid_7
forecast(sarima_7,h=12)
jarque.bera.test(resid_7)

sarima_8=arima(y,order = c(2,1,1),seasonal = list(order=c(2,1,1),period=1))
sarima_8
resid_8=sarima_8$residuals
resid_8
forecast(sarima_8,h=12)
jarque.bera.test(resid_8)

sarima_9=arima(z,order = c(2,1,1),seasonal = list(order=c(2,1,1),period=1))
sarima_9
resid_9=sarima_9$residuals
resid_9
forecast(sarima_9,h=12)
jarque.bera.test(resid_9)

#calculate SSE for c(2,1,1)
SSE_7=sum(sarima_7$residuals^2)
SSE_7
SSE_8=sum(sarima_8$residuals^2)
SSE_8
SSE_9=sum(sarima_9$residuals^2)
SSE_9

##sarima fit for c(2,1,2)
sarima_10=arima(x,order = c(2,1,2),seasonal = list(order=c(2,1,2),period=1))
sarima_10
resid_10=sarima_10$residuals
resid_10
forecast(sarima_10,h=12)
jarque.bera.test(resid_10)

sarima_11=arima(y,order = c(2,1,2),seasonal = list(order=c(2,1,2),period=1))
sarima_11
resid_11=sarima_11$residuals
resid_11
forecast(sarima_11,h=12)
jarque.bera.test(resid_11)

sarima_12=arima(z,order = c(2,1,2),seasonal = list(order=c(2,1,2),period=1))
sarima_12
resid_12=sarima_12$residuals
resid_12
forecast(sarima_12,h=12)
jarque.bera.test(resid_12)

#calculate SSE for c(2,1,2)
SSE_10=sum(sarima_10$residuals^2)
SSE_10
SSE_11=sum(sarima_11$residuals^2)
SSE_11
SSE_12=sum(sarima_12$residuals^2)
SSE_12


##sarima fit for c(0,1,1)
sarima_13=arima(x,order=c(0,1,1),seasonal = list(order=c(0,1,1),period=1))
sarima_13
resid_13=sarima_13$residuals
resid_13
forecast(sarima_13,h=12)
jarque.bera.test(resid_13)

sarima_14=arima(y,order=c(0,1,1),seasonal = list(order=c(0,1,1),period=1))
sarima_14
resid_14=sarima_14$residuals
resid_14
forecast(sarima_14,h=12)
jarque.bera.test(resid_14)

sarima_15=arima(z,order=c(0,1,1),seasonal = list(order=c(0,1,1),period=1))
sarima_15
resid_15=sarima_15$residuals
resid_15
forecast(sarima_15,h=12)
jarque.bera.test(resid_15)

#calculate SSE for c(0,1,1)
SSE_13=sum(sarima_13$residuals^2)
SSE_13
SSE_14=sum(sarima_14$residuals^2)
SSE_14
SSE_15=sum(sarima_15$residuals^2)
SSE_15

##another method of parameter estimator

install.packages("astsa")
library(astsa)

#for c(1,1,1)
sarima(x,1,1,1)
sarima(y,1,1,1)
sarima(z,1,1,1)

#c(1,1,2)
sarima(x,1,1,2)
sarima(y,1,1,2)
sarima(z,1,1,2)

#for c(0,1,1)
sarima(x,0,1,1)
sarima(y,0,1,1)
sarima(z,0,1,1)

#for c(2,1,1)
sarima(x,2,1,1)
sarima(y,2,1,1)
sarima(z,2,1,1)

#for c(2,1,2)
sarima(x,2,1,2)
sarima(y,2,1,2)
sarima(z,2,1,2)

## Q Q plot for c(1,1,2) 
qq_plot=qqnorm(resid_4,main = "qq plot for usd") # USD
qq_plot1=qqnorm(resid_5,main = "qq plot for GBP")# GBP
qq_plot2=qqnorm(resid_6,main = "qq plot for EURo")# EURO

##For influence point checking...

qq_plot=qqnorm(std_resid,main = "qq plot for GBP")

####.........................  outlier detection.... ...................####

install.packages("tsoutliers")
library(tsoutliers)

#### For GBP #### 

outliers_GBP_ts=tso(y,types = c("AO","TC"))
outliers_GBP_ts$times
plot(outliers_GBP_ts)
outliers_GBP_ts$outliers
# time index where the outliers have been detected
outliers_idx <- outliers_GBP_ts$outliers$ind
length(outliers_idx)
# calendar years where the outliers have been detected 
Z=outliers_GBP_ts$outliers$time
Z#length of our time series
n <- length(y)
n

# transient change outlier at the same time index as found for our time series
trans_change <- outliers("TC", outliers_idx)
trans_change
length(trans_change)

# transient change effect data is stored into a one-column matrix, tc
tc_eff <- outliers.effects(trans_change,204)
tc_eff
length(tc_eff)

# converting to a number
coefhat <- as.numeric(unlist(outliers_GBP_ts$outliers["coefhat"]))
coefhat
length(coefhat)

# obtaining the transient change data with same magnitude as determined by the tso() function
tc_effect <- coefhat*tc_eff
tc_effect
length(tc_effect)
# definining a time series for the transient change data
tc_effect_ts <- ts(tc_effect, frequency = 12)
tc_effect_ts
length(tc_effect_ts)

# subtracting the transient change intervention to the original time series, obtaining a time series without the transient change effect
excess_wo_ts <- (y - tc_effect_ts)
length(resid_5)
excess_wo_ts

# plot of the original, without intervention and transient change time series 
plot(y)
lines(excess_wo_ts[,1],type="l",col=2,lwd=2,lty=3)  
lines(excess_wo_ts[,2],type="l",col=4,lwd=2,lty=3)  
plot(cbind(y, excess_wo_ts, tc_effect_ts))



#### FOR USD ####

outliers_USD_ts=tso(x,types = c("AO","TC"))
outliers_USD_ts$times

plot(outliers_USD_ts)
outliers_USD_ts$outliers
# time index where the outliers have been detected
outliers_idx1 <- outliers_USD_ts$outliers$ind
outliers_idx1
# calendar years where the outliers have been detected 
Z_1=outliers_USD_ts$outliers$time
Z_1


# transient change outlier at the same time index as found for our time series
Addit_change <- outliers("AO", outliers_idx1)
Addit_change

# transient change effect data is stored into a one-column matrix, tc
AO_eff<- outliers.effects(Addit_change,204)
AO_eff

# converting to a number
coefhat_1 <- as.numeric(unlist(outliers_USD_ts$outliers["coefhat"]))
coefhat_1

# obtaining the transient change data with same magnitude as determined by the tso() function
AO_effect <- coefhat_1*AO_eff
AO_effect

# definining a time series for the transient change data
AO_effect_ts <- ts(AO_effect, frequency = 12)
AO_effect_ts

# subtracting the transient change intervention to the original time series, obtaining a time series without the transient change effect
excess_wo_ts_1 <- (x-AO_effect_ts)
excess_wo_ts_1

# plot of the original, without intervention and transient change time series 
plot(x)
lines(excess_wo_ts_1,type="l",col=2,lwd=2,lty=3)  
 
plot(cbind(y, excess_wo_ts_1, AO_effect_ts))

#### FOR EURO ####
outliers_EURO_ts=tso(z,types = c("AO","TC"))
outliers_EURO_ts$times

plot(outliers_EURO_ts)
outliers_EURO_ts$outliers
# time index where the outliers have been detected
outliers_idx2 <- outliers_EURO_ts$outliers$ind
outliers_idx2
# calendar years where the outliers have been detected 
Z_2=outliers_EURO_ts$outliers$time
Z_2


# transient change outlier at the same time index as found for our time series
Addit_change_1 <- outliers("AO", outliers_idx2)
Addit_change_1

# transient change effect data is stored into a one-column matrix, tc
AO_eff_1<- outliers.effects(Addit_change,204)
AO_eff_1

# converting to a number
coefhat_2 <- as.numeric(unlist(outliers_EURO_ts$outliers["coefhat"]))
coefhat_2

# obtaining the transient change data with same magnitude as determined by the tso() function
AO_effect_1 <- coefhat_2*AO_eff_1
AO_effect_1

# definining a time series for the transient change data
AO_effect_ts_1 <- ts(AO_effect_1, frequency = 12)
AO_effect_ts_1

# subtracting the transient change intervention to the original time series, obtaining a time series without the transient change effect
excess_wo_ts_2 <- (x-AO_effect_ts_1)
excess_wo_ts_2
plot(excess_wo_ts_2)
# plot of the original, without intervention and transient change time series 
plot(z)
lines(excess_wo_ts_2,type="l",col=2,lwd=2,lty=3)  

plot(cbind(y, excess_wo_ts_2, AO_effect_ts_1))
