

############### knn
# install.packages("caret")
# install.packages('e1071', dependencies=TRUE)
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
library(caret)
library(e1071)
library(readr)

mobile <- read_csv("train.csv")
glimpse(mobile)
## all variables
mobile$price_range=factor(mobile$price_range)



trctrl <- trainControl(method = "repeatedcv", number = 20, repeats = 4)
set.seed(3333)

knn_fit <- train(price_range ~., data = mobile, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit
ggplot(data=knn_fit,aes(x=k,y=Accuracy))+
  geom_line()


##### prediction
mobile_test <- read_csv("Copy of test.csv")

 test_pred <- predict(knn_fit, newdata = mobile_test)
 test_pred

table(test_pred)

#################################################################################################
### delete variables
mobile1=mobile %>% select(-n_cores,-talk_time,-three_g,-touch_screen)
mobile1$price_range=factor(mobile1$price_range)
trctrl <- trainControl(method = "repeatedcv", number = 20, repeats = 4)
set.seed(3333)
knn_fit <- train(price_range ~., data = mobile1, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit

#### prediction
mobile_test1 <- read_csv("Copy of test.csv")

test_pred1 <- predict(knn_fit, newdata = mobile_test1)
test_pred1

table(test_pred1)
