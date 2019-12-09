## setup
options(stringsAsFactors = FALSE)
library(readr)
library(ggplot2)
library(cluster)   
library(factoextra)    
library(tidyverse)
library(corrplot)
library(rpart)
# read train data
data = read_csv("train.csv")

#decision tree w/ all variables
data$price_range = as.factor(data$price_range)
test = read_csv('test.csv') %>% select(-id)

decision_tree =  rpart(price_range ~ ., data = data)
summary(decision_tree)

data$pred = predict(decision_tree, newdata = data, type = 'class')
f_meas(data, as.factor(price_range), pred)
test$pred = predict(decision_tree, test, type = 'class')
table(test$pred)
#decision tree w/ not all variables
mobile_n = data %>% select(-n_cores, -talk_time, -three_g, -touch_screen)
mobile_n$price_range = as.factor(mobile_n$price_range)
test = read_csv('test.csv') %>% select(-id)

decision_tree =  rpart(price_range ~ ., data = mobile_n)
summary(decision_tree)

mobile_n$pred = predict(decision_tree, newdata = mobile_n, type = 'class')
f_meas(mobile_n, as.factor(price_range), pred)
test$pred = predict(decision_tree, test, type = 'class')
table(test$pred)
#######try
mylogit <- glm(price_range ~ ., data = train, family = 'binomial')
train$pred1 <- predict(mylogit, newdata = train, 
                          type = "response")

#######################kmeans
mobile_s = scale(mobile_n)

fviz_nbclust(mobile_s, kmeans, 
             method = 'silhouette', 
             k.max = 10)

fviz_nbclust(mobile_s, kmeans, 
             method = 'wss', 
             k.max = 10)
##########################
#heatmap
mobile_range = data %>% 
  group_by(price_range) %>% 
  skimr::skim_to_wide() %>% 
  select(price_range, variable, mean) %>% 
  mutate_at(vars(mean), as.numeric)
range_dat = mobile_range %>% 
  pivot_wider(names_from = variable, 
              values_from=mean)
heatmap(as.matrix(range_dat), 
        scale="column", 
        Colv = NA, 
        Rowv = NA)
