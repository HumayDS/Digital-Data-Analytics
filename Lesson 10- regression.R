library(readr)
library(dplyr)
#Datani cekirik
df <- read.csv('https://raw.githubusercontent.com/justmarkham/scikit-learn-videos/master/data/Advertising.csv')
df %>%  head()
## Datada Temzlik apaririq (Sade)
df %>%  is.na() %>%  sum()
df %>%  glimpse()
df %>%  summary()
df <-  df  %>%  filter(Newspaper < 100)

df %>%  summary()

df %>%  head()

df <-  df %>%  select(-X)
df %>%  head()


## Deyishkenler arasinda korrelyasiyaya baxmaliyiq
##Musteqil deyishenler oz aralarinda yuksek elaqede olmnamalidir (MUlticollinearity)

df %>%  cor()

### Elaqeye vizual baxaq
plot(df$TV,df$Sales)

## Sales ve TV arasinda sade modeli quraq
lm(Sales ~ TV, data = df)


# Qurdugumuz modeli daha derin analiz edek
model1 = lm(Sales ~ TV, data = df)
### Options scipen scientific reqemlerin qarsisini almaq ucun istifade edilir
options(scipen = 999)
#Model neticesine baxaq
summary(model1)

#Metrics0--- RMSE ve MSE metrikleri vasitesi ile modeli deyerlendirek 
mse1 <-  sum(model1$residuals**2)/length(model1$residuals)
rmse1 <-  sqrt(mse1)
rmse1
## Orta satisha baxaraq xetani deyerlendirek
mean(df$Sales)


# Residuals - xetalari gorsedir
#Estimate - regressiya emsallari
# STd error - regressiya emsallarinin yayinmasi
# t table dan elde  edilen deyerler
# pr(> t|t)---p value 
# p deyerinin 0.05den az olmasi emsalin anlamli olmasi demekdir
# F statistik p value - modelin umumilikde anlamliligini teyin edir. 
#Eger 0.05 den azdirsa , model anlamlidir
# R squared(1 asili,1 musteqil deyishen olarsa) -  0.46--- Musteqil deyishen, asili deyisheni 46% ifade edir.
#Adjusted R squared -- (1 asili, 2 ve daha cox musteqil deyishen olarsa)


## 2-ci modeli butun musteqil deyisenler vasitesi ile qururuq
model2 = lm(Sales~ . , data = df)
summary(model2)


mse2 <-  sum(model2$residuals**2)/length(model2$residuals)
rmse2 <-  sqrt(mse2)
rmse2



#Remove intercept
model3 =lm(Sales ~ 0 +. , data = df)
summary(model3)

mse3 <-  sum(model3$residuals**2)/length(model3$residuals)
rmse3 <-  sqrt(mse3)
rmse3



model4 = lm(Sales~ TV + Radio , data = df)
summary(model4)


mse4 <-  sum(model4$residuals**2)/length(model4$residuals)
rmse4 <-  sqrt(mse4)
rmse4

##Predicted olunmus deyerler
y_pred = predict(model2)
y_pred

## Real deyerler

df$Sales

## Model uzerinden proqnoz 
## ya datada  olan deyishenler, yada modelin errorlari normal dagilim gostermelidir
## Deyiskenler arasinda elaqe xetti(linear olmalidir)

plot(df$TV , df$Sales)
plot(log1p(df$TV) , log1p(df$Sales))

model_log = lm(log1p(Sales)~ log1p(TV) + log1p(Radio) + log1p(Newspaper), data = df)
summary(model_log)
summary(model2)
predicted <-  predict(model_log)
errors_log = df$Sales - expm1(predicted)


mse_log <-  sum(errors_log**2)/length(errors_log)
rmse_log <-  sqrt(mse_log)
rmse_log

sum(model2$residuals**2)
sum(errors_log**2)

###


#make this example reproducible
set.seed(123)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
train  <- df[sample, ]
test   <- df[!sample, ]


model_train <-  lm(Sales~. , data = train)
summary(model2)

mse_train <-  sum(model_train$residuals**2)/length(model_train$residuals)
rmse_train <-  sqrt(mse_train)
rmse2

##PRedict test set

model_pred <-  predict(model_train, test)
model_pred
test$Sales

model_pred_data <- data.frame('actuals' = test$Sales , 'predictions' = model_pred)

model_pred_data

model_pred_error <-  model_pred_data$actuals - model_pred_data$predictions

mse_test <-  sum(model_pred_error^2) / nrow(model_pred_data)
rmse_test <-  sqrt(mse_test)
rmse_test


## lOGARITMIK MODEL UCUN TEST-TRAIN UZERINDEN DEYERLENDIREK

model_log = lm(log1p(Sales)~ log1p(TV) + log1p(Radio) + log1p(Newspaper), data = train)

model_log_pred <-  expm1(predict(model_log, test))

model_pred_log_data <- data.frame('actuals' = test$Sales , 'predictions' = model_log_pred)
model_pred_log_data

model_pred__log_error <-  model_pred_log_data$actuals - model_pred_log_data$predictions
model_pred__log_error

mse_log_test <-  sum(model_pred__log_error^2) / nrow(model_pred_log_data)
rmse_test <-  sqrt(mse_log_test)
rmse_test















