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
library(ggthemes)
library(randomForest)
library (ROCR)
require(caTools)
set.seed(1234)

##################################################################################################
# random forest
mobile <- read_csv("train.csv")
glimpse(mobile)
mobile=mobile %>% select(-n_cores,-talk_time,-three_g,-touch_screen)
mobile$price_range=factor(mobile$price_range)

sample = sample.split(mobile$price_range, SplitRatio = .75)
train1 = subset(mobile, sample == TRUE)
train2  = subset(mobile, sample == FALSE)

dim(train1)
dim(train2)



# f1 <- as.formula(price_range ~ .,data=train1)

fit_rf <- randomForest(price_range~.,
                       data=train1,
                       ntree=500,
                       do.trace=F)
varImpPlot(fit_rf)

pred = predict(fit_rf, newdata=train2[-17], type = "class")
pred
price_range2=train2$price_range

pred1=table(price_range2,pred)
### f-1 score 

yardstick::accuracy(pred1)


### test predict
mobile_test <- read_csv("Copy of test.csv")


mobile_test <- mobile_test %>%  select(-n_cores,-talk_time,-three_g,-touch_screen)




pred_test <- predict(fit_rf,newdata = mobile_test,type = "class")

mobile_test$price_range=pred_test
View(mobile_test) 

#####################################################################################################
###################################################################################################


### all variables
mobile1 <- read_csv("train.csv")
mobile1$price_range=factor(mobile1$price_range)
sample1 = sample.split(mobile1$price_range, SplitRatio = .75)
train3 = subset(mobile1, sample1 == TRUE)
train4  = subset(mobile1, sample1 == FALSE)
dim(train4)

rf <- randomForest(price_range~.,
                       data=train3,
                       ntree=500,
                       do.trace=F)
varImpPlot(rf)

pred1 = predict(rf, newdata=train4[-21],type="class")

price_range4=train4$price_range
cm = table(price_range4, pred1)

### f-1 score
yardstick::accuracy(cm)

### test predict
mobile_test1 <- read_csv("Copy of test.csv")

pred_test <- predict(rf,newdata = mobile_test1)

mobile_test1$price_range=pred_test
View(mobile_test1) 

