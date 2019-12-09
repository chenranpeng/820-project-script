## setup
library(readr)
options(stringsAsFactors = FALSE)
options(digits=3)
library(dplyr)
library(ggplot2)
library(readxl)
library(dummies)    
library(philentropy)   
library(skimr)     
library(cluster)   
library(factoextra)    
library(dendextend)     
library(devtools)    
library(dplyr)
library(purrr)
library(tidyverse)
library(factoextra)
library(skimr)
library("corrplot")
library(cluster)
library(factoextra)
library(xgboost)

data = read_csv("BA820/team1group/mobile-price-classification/train.csv")
data = data %>% select(-price_range)


### xgboost without selected redundant variables 
set.seed(123)
data1 = data %>% select(-c(n_cores, talk_time, three_g, touch_screen))
SAMP = sample(1:nrow(data1), 2000*0.8)
train1 = data1[SAMP, ]
train2 = data1[-SAMP, ]
datatrain = read_csv("BA820/team1group/mobile-price-classification/train.csv")
prange_train1 = datatrain$price_range[SAMP]
prange_train2 = datatrain$price_range[-SAMP]


train1 = as.matrix(train1)
xgmod = xgboost(data = train1,
                label = prange_train1,
                objective = "multi:softmax",
                num_class = 5,
                nrounds = 15)
train2 = as.matrix(train2)
preds = predict(xgmod, train2)
xgtab = table(preds, prange_train2)
xgtab
## accuracy
yardstick::accuracy(xgtab)



### xgboost with all variables 
SAMP = sample(1:nrow(data), 2000*0.8)
train3 = data[SAMP, ]
train4 = data[-SAMP, ]
datatrain = read_csv("BA820/team1group/mobile-price-classification/train.csv")
prange_train3 = datatrain$price_range[SAMP]
prange_train4 = datatrain$price_range[-SAMP]


train3 = as.matrix(train3)
xgmod1 = xgboost(data = train3,
                label = prange_train3,
                objective = "multi:softmax",
                num_class = 5,
                nrounds = 15)
train4 = as.matrix(train4)
preds1 = predict(xgmod, train4)
xgtab1 = table(preds1, prange_train4)
xgtab1
## accuracy
yardstick::accuracy(xgtab1)

## lets test with xgboost having all variables
test = read_csv("BA820/team1group/mobile-price-classification/test.csv")

test1 = test %>% select(-id)
test1 = as.matrix(test1)
View(test1)
preds = predict(xgmod1,newdata = test1)
class(preds)

final_test = as.data.frame(test1) %>%
  mutate(id=test$id) %>% 
  mutate(prince_range = preds)

## result of test data with new price range 
View(final_test)
table(preds)
preds 
class(preds)
preds = as.data.frame(preds)
ggplot(preds, aes(x=as.factor(preds), fill=as.factor(preds))) +
  geom_histogram(stat = 'count', show.legend = F) +
  xlab("New Price Range") 


