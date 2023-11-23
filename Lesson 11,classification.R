# # 
# # install.packages('caret')
# install.packages('glmnet')
# install.packages("ROCR")


library(readr)
library(dplyr)
library(caret)
library(glmnet)
library(ROCR)

df <-  read.csv('https://raw.githubusercontent.com/HumayDS/Big-data-analysis/main/diabetes.csv')

df %>% head()

## Datada Temzlik apaririq (Sade)
df %>%  is.na() %>%  sum()
df %>%  glimpse()
df %>%  summary()

df_2 <-  df   %>%  filter(DiabetesPedigreeFunction < 2 )

df_2 %>%  summary()

df_2 <- df_2 %>%  select(-X)

df %>%  cor()

#make this example reproducible
set.seed(123)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df_2), replace=TRUE, prob=c(0.8,0.2))
train  <- df_2[sample, ]
test   <- df_2[!sample, ]


modellogit <-  glm(Outcome ~ ., data = train, family = 'binomial' )

summary(modellogit)

##Proqnoz deyerler train ucun

predict(modellogit, train , type = 'response')
predictions_prob <-  predict(modellogit, train , type = 'response')



# Changing probabilities
predictions <- ifelse(predictions_prob >0.5, 1, 0)

table(train$Outcome, predictions)

## Calculating accuracy
missing_classerr <- mean(predictions != train$Outcome)
print(paste('Accuracy =', 1 - missing_classerr))

##Calculate manually
(356+118)/609


##Proqnoz deyerler test ucun

predict(modellogit, test , type = 'response')
predictions_prob <-  predict(modellogit, test , type = 'response')



# Changing probabilities
predictions <- ifelse(predictions_prob >0.5, 1, 0)

# ##Accuracy
# #Accuracy deyerini hesablayaq 
# (butun duz proqnoz edilenler / butun proqnoz edilenler 
# ----Proqnoz etdiklerimizin 77%-i duzdur)
table(test$Outcome, predictions)
cm <-  table(test$Outcome, predictions)

(cm[1,1] + cm[2,2]) / sum(cm)
(89+31)/155

## Calculating accuracy
missing_classerr <- mean(predictions != test$Outcome)
print(paste('Accuracy =', 1 - missing_classerr))

summary(predictions_prob)


### Optimum Cutoff

pred_test <- predict(modellogit,test,type="response")
ROCR_pred_test <- prediction(pred_test,test$Outcome)
ROCR_perf_test <- performance(ROCR_pred_test,'tpr','fpr')
plot(ROCR_perf_test,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
ROCR_pred_test@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]


# #Presicion-- Deqiqlik --- Diabet olaraq proqnoz etdiklerimizin 
# nece faizi realda diabetdir ?
# # TP/(TP+FP)
cm
#34/(34+20)
cm[2,2]/(cm[2,2] + cm[2,1])
presicion = cm[2,2]/(cm[2,2] + cm[2,1])

# 
# #Recall -  Sensitivity  -- True Pozitive rate --  
# Realda diabet olanlarin nece faizi diabet olaraq proqnoz edildi?
# #TP/(TP+FN)
cm
#34/(34+14)
cm[2,2]/(cm[2,2] + cm[1,2])
recall = cm[2,2]/(cm[2,2] + cm[1,2])

# F1 Skoru --  2 * (Presicion * recall)/(Presicion + recall)
#Presicion ve Recallin balanslasdirilmis(hormonlasdirilmis) deyeri

2*(presicion *recall)/(presicion + recall)

















