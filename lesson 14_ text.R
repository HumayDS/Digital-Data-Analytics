library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

##Datalar chekilir
bikes <- read.csv('https://raw.githubusercontent.com/HumayDS/Big-data-analysis/main/bikes_big_dt.csv')
bikeshop <- read.csv('https://raw.githubusercontent.com/HumayDS/Big-data-analysis/main/bikeshop_big_dt.csv')
orderlines <- read.csv('https://raw.githubusercontent.com/HumayDS/Big-data-analysis/main/orderlines_big_dt.csv')
## Lazimsiz columnlar silinir
bikes <-  bikes %>%  select(-1)

bikeshop <-  bikeshop %>%  select(-1)

orderlines <-  orderlines %>%  select(-1)

## Achar columnlara gore datalar birlesdirilir
merged_data <-  left_join(bikes,orderlines,by = c('product.id' = 'product.id'))
merged_data <-  left_join(bikeshop,merged_data,by = c('customer.id' = 'customer.id'))
df <- merged_data
glimpse(merged_data)

#customer.id unikal deyerlerin sayina baxiram
n_distinct(df$customer.id)
## location icerisinde unikal deyerlere baxiram
unique(df$location)
#butun kolumnlar uzre unikal deyerlerin sayina baxiram
sapply(df, function(x) length(unique(x)))

##column adlarina baxaq
colnames(df)

##column adlarinda noqteni alt xettle evez edirem
colnames(df) <-  gsub('\\.' , '_',colnames(df))
df %>%  glimpse()


##order_date-in tipini deyisib date qoyuram
df$order_date<- as.Date(df$order_date)
df %>%  glimpse()

## date-den il, ay, hefte, heftenin gunlerini cekirem
df$month<- month(df$order_date,label = TRUE)
df$week<- week(df$order_date)
df$year<-year (df$order_date)

df$day_of_week <-  format(df$order_date,'%A')

## qiymeti saya vuraraq odenish meblegini tapiram ve df-e elave edirem
df$revenue <-  df$price * df$quantity

##Heftenin gunlerine gore ortalama odenish meblegini 
#qruplasdirib azdan choxa siralayiram
df %>%  group_by(day_of_week) %>%  summarise(mean_revenue = mean(revenue)) %>% 
    arrange(desc(mean_revenue))


##df-de revenue uzre coxdan aza siralayiram
df %>%  arrange(desc(revenue))


## text tipli columnlari , balaca herfle  bouk herfle , bash herfleri boyukle yaziram
str_to_lower(df$bikeshop_name)
str_to_upper(df$bikeshop_name)
str_to_title(df$bikeshop_name)

#dfe column elave edirem bikeshop_name balaca herfle yazilsin
df$bikeshop_name_lower <-  str_to_lower(df$bikeshop_name)
library(tidyr)
#separate column
###location column-u vergulle 2 ayri columna ayirriam
separate(df , location , into = c('loc1','loc2'), sep = ',')

df <-  separate(df , location , into = c('loc1','loc2'), sep = ',' ,remove = FALSE)
df$location
df %>%  glimpse()
##Replacing--locationda NY yazilani AY ile evezleyirem

str_replace(df$location,'NY' , 'AY')

## datasetiin 1544-15644 setirlerini kesib df_cut kimi yadda saxlayiram
df_cut <-  df[15444 : 15644 , ]

## df-den hemin setirleri cixarib yadda saxlayiram
df_cut_1 <-  df[-(15444 : 15644) , ]

#2 data frami alt alta birlesdirirem 
df2 <-  rbind(df_cut_1 , df_cut)
# dfden locationi cekib df_loc kimi yadda saxlayiram
df_loc  <-  df$location 
##dfde locatinu silirem
df <-  df %>%  select(-location)
## sutun uzre 2 dataframi birlesdirirem
df <-  cbind(df_loc, df)

##df-de yeni price_cat columnu yaradiram, price 415-1840 olana az,
#1840-2700 olana orta, 2700 den boyuk olana cox yazdiriram
df <-  df %>%  mutate(price_cat  =  case_when(
   price >= 415 & price < 1840 ~ 'az' , 
   price >=1840 & price<=2700 ~'orta' , 
   price > 2700 ~'chox'
))

df %>%  glimpse()

##price orta olanlari filtrleyirem
df %>%  filter(price_cat == 'orta')

##dfde olan loc1 veloc2 columnu tire(-) ile birlesdirirem 
paste(df$loc1 , '-' , df$loc2)
#dfde paste1 kimi yadda saxlayiram
df$paste1 <-  paste(df$loc1 , '-' , df$loc2)
glimpse(df)

##il ile ayi tire ile bir columnda birlesdirirem
paste(df$year , '-' , df$month)
##column adlarina suffix elave edirem

setNames(df , paste0(names(df), '_bikes'))
## yadda saxlayiram 
df <- setNames(df , paste0(names(df), '_bikes'))
#baxiram
df %>%  glimpse()


write.csv(df , 'df_clear.csv')
