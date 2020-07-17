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

# 2. Settings (Local Access)
db_user2 <- 'root'
db_password2 <- 'Gumdrop89!'
db_name2 <- 'imarket_sql'
db_table2 <- 'line_item'
db_host2 <- '127.0.0.1' # for local access
db_port2 <- 3306

# 3. Read data from db
mydb2 <-  dbConnect(MySQL(), 
                    user = db_user2, 
                    password = db_password2,
                    dbname = db_name2, 
                    host = db_host2, 
                    port = 3306)

rs2 <- dbSendQuery(mydb2, "select * from line_item")
lineitem_data <-  fetch(rs2, n = -1)
dbListTables(mydb2)

#products
rs3 <- dbSendQuery(mydb2, "select * from products")
products_data <-  fetch(rs3, n = -1)

#Orders
rs4 <- dbSendQuery(mydb2, "select * from orders")
orders_data <-  fetch(rs4, n = -1)

on.exit(dbDisconnect(mydb2))

######################################

# Match all orders in line_item  in orders dataset. 
#Exclude any rows that do not meet that condition.

line2 <- inner_join(lineitem_data, orders_data, by = "id_order")

#Exclude any rows from orders that are not “Completed”.
line_completed <- line2 %>% 
  filter(state == "Completed")

#Check  all products are present in the products dataset. 
#Exclude any rows that do not meet that condition.

line_prod <- inner_join(line_completed, products_data, by = "sku")

#Explore the relationship between prices in line_item and order:
#Create a new table with unit price, product quantity and total price
price <- select(line_prod, ProductId, id_order, unit_price, product_quantity, total_paid)

#replace comma with decimal point
price$unit_price <- as.numeric(gsub(",", ".", gsub("\\.", "", price$unit_price)))
price$total_paid <- as.numeric(gsub(",", ".", gsub("\\.", "", price$total_paid)))

#convert to dataframe
price <- as.data.frame(price)

#add new column calculating the total price based on unit_price*product_quantity
price <- price %>% 
  mutate(totalpaid_estimate = unit_price*product_quantity)

#group total price column by id_order
price_compare <- select(price, id_order, total_paid) %>% 
  group_by(id_order) %>% 
  summarise_at(vars(total_paid), funs(mean))

total_compare <- select(price, id_order,product_quantity, totalpaid_estimate) %>% 
  group_by(id_order) %>% 
  summarise_at(vars(product_quantity, totalpaid_estimate), funs(sum))

#join tables total_compare and price_compare to examine price difference
total_compare <- inner_join(total_compare, price_compare, by = "id_order") 

#add column with absolute, rounded price difference between out estimate and the total paid from line_item dataframe
options(scipen=30, digits = 2)
total_compare <- total_compare %>% 
  mutate(total_diff = abs(total_paid - totalpaid_estimate))

total_compare <-total_compare %>% 
  mutate(total_diff = round(total_diff))

#Exclude any rows where price difference is over 30 euros
total_compare <-filter(total_compare, total_diff< 30) 

#join sku column onto total_compare
clean_lineitems <- inner_join(total_compare, line_prod, by = "id_order") 

#remove low frequency products (i.e. anything below 12 counts)
clean_lineitems2 <- clean_lineitems %>% 
  group_by(sku) %>% 
  filter(n() > 12)

#Write a csv file out of the line_item dataframe (the one that you’ve been cleaning), 
#containing only the columns id_order and sku. 

write.csv(clean_lineitems[,c("id_order", "sku")], file="sku_order.csv",row.names=FALSE)
write.csv(clean_lineitems2[,c("id_order", "sku")], file="sku_order2.csv",row.names=FALSE)

#Investigating association between different product categories, in addition to product types

#merge column with categories and brand information in cleanitems dataframe
clean_lineitems3 <- mutate(clean_lineitems2, brand_accesories = paste(brand, "-", manual_categories)) 

#Second csv file to compare association between product categories
write.csv(clean_lineitems3[,c("id_order", "brand_accesories")], file="sku_order3.csv",row.names=FALSE)

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

