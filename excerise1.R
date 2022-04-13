#installing required packages
install.packages("tidyverse")
install.packages("dplyr")

#loading necessary libraries
library(ggplot2)   
library(tidyr)
library(dplyr)
library(tidyverse) 
library(lubridate)


setwd("D:/Rhine-waal Hsrw university/Datamining")  # set working directory
data <- read.csv("Web_Baskets_2020.csv", sep = ";")   #loading data-set into variable data
head(data) #checking first 6 rows of the data

dim(data)  #checking dimension of the data (3249944 rows, 6 column)

# sum of quantity per category
df1 <- data %>%
  group_by(article_cat) %>%
  summarise(total_quantity = sum(quantity)) %>%
  arrange(desc(total_quantity))

# top 5 article per category
df2 <- data %>%
  group_by(article_cat) %>%
  summarise(quantity = sum(quantity)) %>%
  arrange(desc(quantity))%>%
  top_n(5,quantity) 

# top 5 article category per day
df3 <- data %>%
  group_by(date, article_cat) %>%
  summarise(quantity = sum(quantity)) %>%
  arrange(desc(quantity))%>%
  top_n(5,quantity) 

#df4<- df3[!df3$article_cat %in% c("terry goods","cleaning aids", "bedsheet"), ] #remove specific row from df3
head(df4) 


#visualize the best selling article of each of the five categories per day in one graph
install.packages("patchwork")
library(patchwork)
ggplot(df4, aes(x = date, y = quantity, fill = article_cat)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  labs(x = "Dates(2020-04-01 till 2020-05-01)", y = "total quantity per day" , title = "Best-selling articles of the each of the five category per day") +
  facet_wrap(vars(article_cat), scales="free", ncol=2)

#ggplot2
ggplot(df4,aes(x = date, y = quantity, col = article_cat))+geom_point(size = 4)+facet_wrap(vars(article_cat))+ 
  scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE)]})+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) + 
  labs(title = "Best-selling articles based per day",
       x = "Date",
       y = "Number of quantity") +theme_bw() +theme(text=element_text(size = 16))

#ggplot3
ggplot(df4,aes(x = date, y = quantity, col = article_cat, fill = article_cat))+  geom_bar(stat="identity") + 
   labs(x = "Dates", y = "Number of quantity" , title = "Best-selling articles of the each of the five category per day") +
   theme(strip.text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)) +
  facet_wrap(vars(article_cat), scales="free", ncol=2)  





       
