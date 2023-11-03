library(readr)
library(dplyr)
df <- read.csv('https://raw.githubusercontent.com/justmarkham/scikit-learn-videos/master/data/Advertising.csv')
df %>%  head()
df %>%  is.na() %>%  sum()
df %>%  glimpse()
df %>%  summary()
df2 <-  df  %>%  filter(Newspaper < 100)



df %>%  head()

df <-  df %>%  select(-X)
df %>%  head()


plot(df$TV,df$Sales)

lm(Sales ~ TV, data = df)



model1 = lm(Sales ~ TV, data = df)
options(scipen = 999)
summary(model1)

#Metrics 
mse1 <-  sum(model1$residuals**2)/length(model1$residuals)
rmse1 <-  sqrt(mse1)
rmse1




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


#Checking multicollinearity
cor(df)
model2 = lm(Sales~. , data = df)
summary(model2)




mse2 <-  sum(model2$residuals**2)/length(model2$residuals)
rmse2 <-  sqrt(mse2)
rmse2



# eger musteqil deyishenler arasinda yuksek elaqe varsa, 
#burada multicollinearity problemi movcuddur

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



#Checking heteroskedasticity(deyisen varians)
# Xetalar

errors <- model$residuals
plot(errors)
plot(model2)
hist(errors)



# Modelin xetalari randomly dagilim gorsetmelidir. 
# Ya random paylanmali, ya da bir yere toplasmalidirlar. 
#eyer artim ve ya azalma kimi plot gorsenerse, burada deyisen varians problemi 
#vardir. Model yaxsi netice gostermez bu halda
#  Bu problemi logaritmik transformasiya hell ede biler
# Scatter plotla gore bilmesek, preush pagan veya white testi ile test ede bilerik

#install.packages('lmtest')
library(lmtest)
lmtest::bptest(model2)
#p value 0.05 den azdirsa , heteroscedasticity problemi var(deyishen varians)
#p value 0.05den coxdursa, hetero.problemi yoxdur, homoscedastic modeldir


















