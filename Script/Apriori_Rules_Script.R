#Author: Anita Rijal
#Version: 1
#Date: March 6, 2020
#Description: Market Basket Analysis to understand association between product types using apriori rules

# 1. Library
library(RMySQL)
library(dplyr)
library(tidyverse)
library(arules)
library(arulesViz)

#Read the csv file that you created using the read.transactions from arules. 
sku_order<- read.transactions("sku_order.csv", format = "single", 
                              sep = "," , header = TRUE, 
                              cols= c("id_order","sku"))

sku_order2<- read.transactions("sku_order2.csv", format = "single", 
                               sep = "," , header = TRUE, 
                               cols= c("id_order","sku"))

sku_order3<- read.transactions("sku_order3.csv", format = "single", 
                               sep = "," , header = TRUE, 
                               cols= c("id_order","brand_accesories"))
#Investigating frequency of products by top 20 products
itemFrequency(sku_order, type = "absolute")

#visualising product frequency by top 20
itemFrequencyPlot(sku_order , topN=20 , type = "absolute")

#top 5 categories
itemFrequencyPlot(sku_order3 , topN=5 , type = "absolute")

#least frequent products and categories
barplot(sort(itemFrequency(sku_order2), decreasing=F))
barplot(sort(itemFrequency(sku_order3), decreasing=F))

#APRIORI rules
rule1 <- apriori(sku_order2, parameter = list(support = 0.0006, confidence = 0.3))
summary(rule1)
inspect (rule1)
summary(sku_order)

#visualise rules
plot(rule1)
plot(rule1, method = "graph")
plot(rule1, method = "paracoord", control = list(reorder = TRUE)
     
rule2 <- apriori(sku_order2, parameter = list(supp=0.0004, conf=0.2,maxlen=10))
summary(rule2)
inspect(rule2)
     
     plot(rule2, method = "graph")
     plot(rule2, method = "paracoord", control = list(reorder = TRUE))
     
     #categories association rule
     rule4 <- apriori(sku_order3, parameter = list(support = 0.0006, confidence = 0.4))
     inspect(rule4)
     
     plot(rule4, method = "graph")
     plot(rule4, method = "paracoord", control = list(reorder = TRUE))
     
     