library(readr)
library(dplyr)
library(lubridate)
library(tseries)

df <-  read_csv('https://raw.githubusercontent.com/HumayDS/Digital-Data-Analytics/main/ts_data.csv')
df <-  sample_n(df, 2000)


df %>%  glimpse()
write.csv(df,'sales.csv')
df$date<- as.Date(df$InvoiceDate)
df$month<- month(df$date,label = TRUE)
df$week<- week(df$date)
df$year<-year (df$date)

df %>%  summary()

df %>% glimpse()


### Removing outlier  with IQR

Q1 <-  quantile(df$Quantity , probs = 0.25)

Q3 <-  quantile(df$Quantity , probs = 0.75)

IQR = Q3 - Q1
upper_bound <-  Q3 + 1.5* IQR

lower_bound <-  Q1 - 1.5*IQR

outlier_ind <- which(df$Revenue < lower_bound | df$Revenue > upper_bound)

outlier_ind

df <-  df[-outlier_ind,]



for_monthly<- df %>% group_by(month,year) %>% 
  summarise(sales= sum(Quantity,na.rm = TRUE)) %>% arrange(year,month)

for_weekly<-df %>% group_by(week,year) %>% 
  summarise(sales= sum(Quantity,na.rm = TRUE)) %>% arrange(year,week)

for_daily<- df %>% group_by(date,year) %>% 
  summarise(sales= sum(Quantity,na.rm = TRUE)) %>% arrange(year,date)

library(forecast)

time_series_monthly<- ts(for_monthly[,3],start = c(2009,12),frequency = 12)

autoplot(time_series_monthly)




#### Seasonality and AUTO-correlations

ggseasonplot(time_series_monthly)


### Auto_corelations

ggAcf(time_series_monthly,lag.max = 24)

## Decomposition -- season, trend , remainder 
decompsition<- stl(time_series_monthly[,1],s.window = "periodic")

decompsition

autoplot(decompsition)



###trend
trend_series<-trendcycle(decompsition)

autoplot(trend_series)

### seasonality

seasonal_series<- seasonal(decompsition)


autoplot(seasonal_series)


###remainder
##Data cannot explain in mathmatical terms stays in residual
rem_series<-remainder(decompsition)

autoplot(rem_series)


### stregnth of seasonality and trend

trend_stregnth<- max(0,1-((var(rem_series)/(var(rem_series+trend_series)))))

trend_stregnth

seas_stregnth<- max(0,1-((var(rem_series)/(var(rem_series+seasonal_series)))))
seas_stregnth
#install.packages('hts')
library(hts)
time_series_monthly

train<- window(time_series_monthly,end=c(2011,7))
train
length(train)
length(time_series_monthly)

#Dickey fuller test for stationarity
##If p value < 0.05 ts is stationary

adf.test(train)


##Smoothing ---  Works best with non seasonal data
## Ideal for getting the trend and removing noise
###Moving average

autoplot(train)
time_series_monthly

#smoothed with moving average
library(quantmod)
SMA(time_series_monthly,2)
ma2 <- SMA(time_series_monthly,2) 
time_series_monthly
ma2
autoplot(time_series_monthly)
autoplot(ma2)


##Exponential smoothing
##Recent data is given more weight than older observation
##ses function, hw(for holt winter)
##ses for Simple exponential smoothing  -
##use it for dataset without seasonality or trend
## It has Holt Winter type for dataset with trend, 
#but without seasonality holt() --Holt exponential smoothing
## Holt Winter hw() for data with trend and seasonality
## Automated model selection -- ets() R select it for you
## ets select model which is best suitable



##Simple Exponential smoothing
##This method is suitable for forecasting data with no clear 
#trend or seasonal pattern.

ses(train)
ses_model <-  ses(train)
ses_model %>% accuracy(time_series_monthly)

###
train %>%  mean()
##Holt model
holt(train,h=5)
holt(train,h=5 ) %>%  accuracy(time_series_monthly)
autoplot(holt(train,h=5 ))
##Holt winter 

hw(train,h=5, alpha = 0.7 , seasonal = "multiplicative")

hw(train,h=5,alpha = 0.7 , seasonal = "multiplicative") %>% accuracy(time_series_monthly)


## ETS (auto selection)

fit_ets<- ets(train)
forecasting<- forecast(fit_ets,h=5)
forecasting
forecasting %>% accuracy(time_series_monthly)



#Arima

arima_model<- auto.arima(train)
arima_model
forecast(arima_model,h=5) 
forecast(arima_model,h=5) %>% accuracy(time_series_monthly)


## myarima MA moving average
myarima<- arima(train , order = c(1,2,3))
forecast(myarima,h=5) 
time_series_monthly
forecast(myarima,h=5) %>% accuracy(time_series_monthly)

autoplot(forecast(myarima,h=5))


SARIMA<- auto.arima(train,xreg = fourier(train,K=6),seasonal = TRUE)
forecast(SARIMA,xreg = fourier(train,K=6,h=5))

forecast_harmonic<- forecast(SARIMA,xreg = fourier(train,K=6,h=1))

autoplot(forecast_harmonic)
forecast_harmonic %>% accuracy(time_series_monthly)


write.csv(forecast_harmonic, 'forecast_harmonic.csv')


time_series_monthly
forecast_harmonic



















