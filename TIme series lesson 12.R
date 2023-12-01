#install.packages('lubridate')
library(readr)
library(dplyr)
library(lubridate)
library(dplyr)
library(ggplot2)

df <-  read_csv('https://raw.githubusercontent.com/HumayDS/Digital-Data-Analytics/main/ts_data.csv')

df %>%  head()
df %>% glimpse()
df %>% is.na() %>% sum()

df %>%  head()
df %>%  summary()

df <-  df %>%  select(-...1 , -X)

df %>%
  select_if(is.character) %>%
  glimpse()

df %>%
  select_if(is.character) %>%
  map(unique)


df %>%
  select_if(is.character) %>%
  map(~ table(.) %>% prop.table())

df %>%
  select_if(is.numeric) %>%
  map(~ unique(.) %>% length())


df %>%
  select_if(is.numeric) %>%
  map_df(~ unique(.) %>% length()) %>%
  gather() %>%
  arrange(value) %>%
  filter(value <= 11)

##as.Date(df$InvoiceDate)
df %>%  summary()

df$week <-  week(df$InvoiceDate)
df$month <-  month(df$InvoiceDate)
df$year <-  year(df$InvoiceDate)

df_monthly<- df %>% group_by(year , month) %>% 
  summarise(revenue= sum(Revenue,na.rm = TRUE)) %>% arrange(year,month)


df_weekly <- df %>% group_by(month,year,week) %>% 
  summarise(revenue= sum(Revenue,na.rm = TRUE)) %>% arrange(year,month,week)

weekly_time <-  df %>% group_by(week,year,month) %>%
  summarise(date=mean(InvoiceDate),
            Revenue=sum(Revenue,na.rm = TRUE)) %>% arrange(date)


weekly_time %>% ggplot(aes(x=date,y=Revenue))+geom_line()


##Working on time_series
weekly_time %>%  glimpse()
weekly_time$date <-  as.Date(weekly_time$date)
weekly_time %>%  glimpse()
weekly_time$trend<- seq(1: nrow(weekly_time))
weekly_time$month<-month(weekly_time$date,label = TRUE)
weekly_time$dayofmonth<- day(weekly_time$date)
weekly_time %>%  glimpse()

weekly_time<-weekly_time %>%
  mutate(first_10= case_when(dayofmonth <= 10 ~ 1,
                             dayofmonth >10 ~ 0))

weekly_time<-weekly_time %>%
  mutate(secoond_10= ifelse(dayofmonth >10 & dayofmonth <=20,1,0))


weekly_time<-weekly_time %>%
  mutate(third_10= ifelse(dayofmonth > 20,1,0))


glimpse(weekly_time)
colnames(weekly_time)
weekly_time <- weekly_time %>%  select(week,month,year, everything())
colnames(weekly_time)
#[,c(2,5,6,8,9,10)]

model_1<- lm(Revenue ~ week + month + year + date+ 
               trend +first_10 + first_10 +secoond_10 + third_10,data = weekly_time)
summary(model_1)


weekly_time$prediction<- predict(model_1,weekly_time)

weekly_time %>%  glimpse()


residuals <-  weekly_time$Revenue-weekly_time$prediction

rmse <- sqrt((sum(residuals**2)) / length(residuals))

rmse


weekly_time %>%  summary()

future_dates<- seq.Date(from = max(weekly_time$date)+7,
                        to= max(weekly_time$date)+ (7*16),by=7)

future_dates

future_dates<- data.frame(date=future_dates)

weekly_time$type<- "History"
weekly_time %>%  glimpse()
nrow(weekly_time)

weekly_time<- rbind(weekly_time,future_dates)
weekly_time %>%  View()


weekly_time$type[is.na(weekly_time$type)==TRUE]<- "Forecast"
table(weekly_time$type)

###redoing it again
weekly_time$trend<- seq(1: nrow(weekly_time))
weekly_time$month<-month(weekly_time$date,label = TRUE)
weekly_time$dayofmonth<- day(weekly_time$date)
weekly_time$year <-  year(weekly_time$date)
weekly_time$week <-  week(weekly_time$date)
weekly_time<-weekly_time %>%
  mutate(first_10= case_when(dayofmonth <= 10 ~ 1,
                             dayofmonth >10 ~ 0))
weekly_time<-weekly_time %>%
  mutate(secoond_10= ifelse(dayofmonth >10 & dayofmonth <=20,1,0))

weekly_time<-weekly_time %>%
  mutate(third_10= ifelse(dayofmonth > 20,1,0))
glimpse(weekly_time)

weekly_time %>%   View()

weekly_time$prediction<-predict(model_1,weekly_time)

a<-weekly_time %>% ggplot(aes(x=date,y=prediction,color=type))+geom_line()+
  geom_line(aes(y=Revenue),color="darkblue")+theme_minimal()

library(plotly)

ggplotly(a)

####### Second method (advanced)

weekly_time_history <-  weekly_time %>% filter(type == 'History')

train_data <-  weekly_time %>%  filter(trend < 101)
test_data <-  weekly_time %>%  filter(trend > 100)
test_data <-  test_data %>%  filter(type == 'History')

model_train<- lm(Revenue ~ week + month + year + date+ 
               trend +first_10 + first_10 +secoond_10 + third_10,data = train_data)


train_data$prediction<- predict(model_train,train_data)
test_data$prediction<- predict(model_train,test_data)
train_data$type <-  'train'
test_data$type <- 'test'

residuals_train <-  train_data$Revenue-train_data$prediction

rmse_train <- sqrt((sum(residuals_train**2)) / length(residuals_train))

rmse_train


residuals_test <-  test_data$Revenue-test_data$prediction

rmse_test <- sqrt((sum(residuals_test**2)) / length(residuals_test))

rmse_test

### Removing outlier  with IQR

Q1 <-  quantile(df$Revenue , probs = 0.25)

Q3 <-  quantile(df$Revenue , probs = 0.75)

IQR = Q3 - Q1
upper_bound <-  Q3 + 1.5* IQR

lower_bound <-  Q1 - 1.5*IQR

outlier_ind <- which(df$Revenue < lower_bound | df$Revenue > upper_bound)
outlier_ind

df <-  df[-outlier_ind,]







