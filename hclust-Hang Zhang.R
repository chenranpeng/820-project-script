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
data = read_csv("train.csv")
skimr::skim(data)
data = data %>% select(-price_range)
stats_z = scale(data)

stats_de = dist(stats_z)
stats_dm = dist(stats_z, method="manhattan")

clust1 = hclust(stats_de)
clust2 = hclust(stats_dm)

plot(clust1)
plot(clust2)

sapply(2:10,function(x) table(cutree(clust2,k=x)))

data$cluster = cutree(clust2, k=4)

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
asdqweasdzxc
ggplot(data,aes(as.factor(cluster),int_memory )) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster), m_dep)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster), mobile_wt)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),  n_cores )) +
  geom_boxplot()

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
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),  three_g)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),  touch_screen)) +
  geom_boxplot()

ggplot(data,aes(as.factor(cluster),  wifi)) +
  geom_boxplot()

# after take off variables
data %>% select(-int_memory,-ram,-px_width,-px_height)
