library(lubridate)
library(readr)
df <-  read.csv('https://raw.githubusercontent.com/HumayDS/Digital-Data-Analytics/main/Churn_Modelling.csv')
df %>%  glimpse()

df %>%  View()

df %>%  glimpse()

df <-  df %>%  select(-RowNumber , -CustomerId , -Surname)


df %>% 
  group_by(Exited) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n / sum(n))



df %>% 
  
  group_by(Gender, Exited) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  group_by(Gender) %>%
  mutate(pct = n / sum(n))


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


df$HasCrCard <-  factor(df$HasCrCard)
df$IsActiveMember <-  factor(df$IsActiveMember)
df$Exited <-  factor(df$Exited)
df$NumOfProducts <-  factor(df$NumOfProducts)
df$Tenure <-  factor(df$Tenure)
df$Gender <-  factor(df$Gender)
df$Geography <-  factor(df$Geography)

df %>%  is.na() %>%  sum()


df %>%  summary()

df %>%  filter(Geography == 'France')

df_num <- df %>%
  select_if(is.numeric)

df_num %>%  cor()

set.seed(123)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
train  <- df[sample, ]
test   <- df[!sample, ]

modellogit <-  glm(Exited ~ ., data = train, family = 'binomial' )

options(scipen = 999)
summary(modellogit)

predictions_prob <-  predict(modellogit, train , type = 'response')

# Changing probabilities
predictions <- ifelse(predictions_prob >0.4855, 1, 0)
table(train$Exited, predictions)
## Calculating accuracy
missing_classerr <- mean(predictions != train$Exited)
print(paste('Accuracy =', 1 - missing_classerr))


## Calculating accuracy
missing_classerr <- mean(predictions != test$Exited)
print(paste('Accuracy =', 1 - missing_classerr))

# Changing probabilities
predictions <- ifelse(predictions_prob >0.443, 1, 0)

missing_classerr <- mean(predictions != test$Exited)
print(paste('Accuracy =', 1 - missing_classerr))

### Optimum Cutoff
library(ROCR)
pred_train <- predict(modellogit,train,type="response")
ROCR_pred_train <- prediction(pred_train,train$Exited)
ROCR_perf_train <- performance(ROCR_pred_train,'tpr','fpr')
cost_perf = performance(ROCR_pred_train, "cost") 
ROCR_pred_train@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]


