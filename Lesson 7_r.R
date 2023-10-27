library(readr)
library(dplyr)

df <-  read_csv("tips_.csv")


# column sec ve filterle


df %>%  filter(tip > 20)

df %>% filter((tip > 15) | (hesab < 50))  #veya

df %>% filter((tip > 15)  & (hesab < 50))  #ve

table(df$cins)

#Missing values 
summary(df)
is.na(df) %>%  sum()

which(is.na(df$tip))

index  <-  which(is.na(df$tip))
index

df[index, ]


# deleting missing values 

na.omit(df)

df_remove_missing <-  na.omit(df)



# Mean Imputation

mean(df$tip , na.rm = T)
ortalama <-  mean(df$tip , na.rm = T)


# Copy dataframe
df_missing_imputation <-  df 

df_missing_imputation$tip[index] <-  ortalama 

df_missing_imputation %>%  summary()

is.na(df_missing_imputation) %>%  sum()


# agregat funskiyalari
df_remove_missing %>%  summary()
#Merkezi tendensiya olculeri 

mean(df_remove_missing$tip)
median(df_remove_missing$tip)
min(df_remove_missing$tip)
max(df_remove_missing$tip)
quantile(df_remove_missing$tip , probs = 0.25 )
quantile(df_remove_missing$tip , probs = 0.75 )
quantile(df_remove_missing$tip  )

#Deyiskenlik olculeri 
sd(df_remove_missing$tip)
var(df_remove_missing$tip)
range = max(df_remove_missing$tip) - min(df_remove_missing$tip)
IQR = quantile(df_remove_missing$tip , probs = 0.75 ) -
  quantile(df_remove_missing$tip , probs = 0.25 )

#outlier detection box plot method


q1 <-  quantile(df_remove_missing$tip ,probs = 0.25)
q3 <-  quantile(df_remove_missing$tip , probs = 0.75)
iqr = q3 - q1

up_bound <-  q3 + 1.5*iqr

low_bound <-  q1 - 1.5*iqr



out_index <-  which(df_remove_missing$tip < low_bound | df_remove_missing$tip > up_bound)

#evezle
df_remove_missing$tip[out_index] <-  ortalama 

#sil
df_clear <-  df_remove_missing[-out_index ,]

df_clear %>%  summary()

#Yuvarlaqlasdir
df_clear$tip <-  round(df_clear$tip)
df_clear %>%  head()

#Korrelyasiya matisi qur

cor(df_missing_imputation$tip , df_missing_imputation$hesab)
cor(df_clear$tip , df_clear$hesab)



#Visualizing

hist(df_clear$hesab , main = "Histogram")

boxplot(df_clear$tip)

boxplot(df$tip , na_rm =T)

#Scatter plot
plot(x = df$tip , y = df$hesab ,pch = 19)

plot(x = airquality$Wind , y = airquality$Temp)

plot(x = airquality$Wind , y = airquality$Temp ,pch = 19 , main = 'Scatter_plot'  ,
     xlab = 'Kuleyin sureti' , 
     ylab = 'Temperatur' , 
     col = 'blue' )


cor(airquality$Wind , airquality$Temp)


Günler = table(df_clear$gun)

Günler

barplot(Günler)
barplot(Günler, names.arg = c('5-ci gun', '6ci gun' , 'bazar' , '4-cu gun'),
        col = 'orange')

cins_table = table(df_clear$cins)
barplot(cins_table)

#Faizle bar plot
prop_cins = prop.table(table(df_clear$cins))
prop_cins
barplot(prop_cins)

?barplot()



#Pie chart basic
pie(prop_cins)


pie(prop_cins , main = 'Cinslere gore bolgu' ,
    col = c('blue', 'red') )




#pivoting

df_clear %>%  group_by(cins) %>% summarize(orta_mean = mean(tip, na.rm = T))

df_clear %>%  group_by(cins) %>%  summarize(cemi_hesab = sum(hesab))  %>%  
  mutate(percent = cemi_hesab/sum(cemi_hesab))
                                    
pie_data <-  df_clear %>%  group_by(cins) %>%  summarize(cemi_hesab = sum(hesab))  %>%  
  mutate(percent = cemi_hesab/sum(cemi_hesab))

#cins uzre hesab - pie chart legend(cins )
pie(pie_data$cemi_hesab , labels = c('Female 34%', 'Male 66%') ,
    main = 'Pie chart')


write_csv(df_clear,'df_clear.csv')
                   