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

# read train data
raw_data = read_csv("820/team project/train.csv")
skimr::skim(data)
data = raw_data %>% select(-price_range)
stats_z = scale(data)

stats_de = dist(stats_z)
stats_dm = dist(stats_z, method="manhattan")

clust1 = hclust(stats_de)
clust2 = hclust(stats_dm)

plot(clust1)
plot(clust2)

sapply(2:10,function(x) table(cutree(clust2,k=x)))

data$cluster = cutree(clust2, k=7)


ggplot(data,aes(as.factor(cluster),battery_power)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),blue)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),clock_speed)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),dual_sim)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),fc)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),four_g)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),int_memory )) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster), m_dep)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster), mobile_wt)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),  n_cores )) +
  geom_boxplot() ##

ggplot(data,aes(as.factor(cluster),  pc)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),  px_height)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),  px_width)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),  ram)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),  sc_h)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),  sc_w)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),  talk_time)) +
  geom_boxplot() ##

ggplot(data,aes(as.factor(cluster),  three_g)) +
  geom_boxplot() ##

ggplot(data,aes(as.factor(cluster),  touch_screen)) +
  geom_boxplot() ##

ggplot(data,aes(as.factor(cluster),  wifi)) +
  geom_boxplot()

# after take off variables
    #data<-data %>% select(-int_memory,-ram,-px_width,-px_height)
data<-data %>% select(-n_cores,-talk_time,-three_g,-touch_screen,-cluster)
#########KNN##########
nrow(data)
set.seed(830)
train_row=sample(1:nrow(data),nrow(data)/2,replace = F)
train=data[train_row,]
test=data[-train_row,]

test_k = function(k) {
  # the model - note train and test are the same
  m = knn(train = train, 
          test = test, 
          cl = as.factor(raw_data$price_range[train_row]), 
          k = k)
  # the test
  dat = data.frame(actual = as.factor(raw_data$price_range[-train_row]),
                   pred = m)
  F1 = f_meas(dat, actual, pred)[3]
  F1 = as.numeric(F1)
  return(F1)
}

x=1:15
f1s = purrr::map_dbl(x, test_k)
plot(x, f1s, 
     main="Eval K for KNN based on F1", 
     type="l",
     ylab="F1 Score",
     xlab="# of Neighbors")
f1_max = which.max(f1s)
f1_max
test_k(3)

#0.9278041
#0.2907421

##predict for test 
train1<-read_csv('820/team project/train.csv')
test1<-read_csv('820/team project/test.csv')
pred_test = knn(train = train1, 
        test = test1, 
        cl = as.factor(train1$price_range), 
        k = 3)
nrow(test1)
length(pred_test)
table(pred_test)








