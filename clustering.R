#Packages Required
install.packages("tidyverse")
install.packages("sqldf")
install.packages("factoextra")
install.packages("dplyr")
library(rmarkdown)
library(sqldf)
library(DT)
library(ggplot2)
library(tidyverse)
library(grid)
library(knitr)
library(dplyr)
library(lubridate)
library(factoextra)
library(gridExtra)
library(fpc)
library(tidyr)
library(cluster)
library(dendextend)

#Data Import
setwd("D:/Rhine-waal Hsrw university/Datamining")  # set working directory
retail <- read.csv("Web_Baskets_2020.csv", sep = ";")   #loading data-set into variable d1
head(retail) #checking first 6 rows of the data
dim(retail)  #checking dimension of the data (3249944 rows, 6 column)

#Structure of the data
str(retail)

#Missing and null values checking:
colSums(is.na(retail))
any(is.null(retail))

#Data Preparation
#retail$date <- as.character(retail$date)
# create month,  day variables
retail$month <- sapply(retail$date, FUN = function(x) {strsplit(x, split = '[-]')[[1]][2]})
retail$day <- sapply(retail$date, FUN = function(x) {strsplit(x, split = '[-]')[[1]][3]})
retail$weekday <-  weekdays(as.Date(retail$date))

#remove NA values
retail <- na.omit(retail)
#Table View of the Data:
datatable(head(retail, 100),extensions = 'FixedColumns', options = list(scrollX = TRUE, scrollY = "400px",fixedColumns = TRUE))

#attriburtes lists
#total units solid by article

df1 <-retail %>% 
  group_by(article_id) %>%
  summarise(totalunitsold = sum(quantity)) %>% 
  arrange(desc(totalunitsold))

#total units sold in weekday per article_id

df2<-retail%>%
  group_by(article_id) %>%
  count(weekday)
df2<- sqldf("select article_id, weekday, max(n)  from df2 group by article_id")
# Rename column where names is "max(n)"
names(df2)[names(df2) == "max(n)"] <- "max_unitsold_week"
head(df2)

#average number of transaction per article per day
df3<-retail %>%
  group_by(article_id) %>%
 summarise(avg_quantity_day=sum(quantity)/31) %>%
arrange(desc(avg_quantity_day))

#total transaction per article per day
df4<- retail%>%
  group_by(article_id,date)%>%
  count(transaction_id)
df4<- sqldf("select distinct article_id, date, count(n) from df4 group by article_id")
# Rename column where names is "count(n)"
names(df4)[names(df4) == "count(n)"] <- "transcation_per_article_perday"
head(df4)


# merging three data frames by article_id

dfmerge1 <- merge(df1, df2, by="article_id")
dfmerge2 <- merge(dfmerge1, df3, by="article_id")
dfmerge3 <- merge(dfmerge2,df4, by="article_id")
dfmerge3$weekday <- NULL
#dfmerge3$date <- NULL
#dfmerge3$article_id <- NULL
head(dfmerge3)
summary(dfmerge3)

#remove outlier
z=dfmerge2.totalunitsold.quantile(0.05)
y=dfmerge2.totalunitsold.quantile(0.95)
iqr=y-z
dfmerge2=dfmerge2[(dfmerge2['totalunitsold']>=z-1.5*iqr) & (dfmerge2['totalunitsold']<=y+1.5*iqr)]

#scaling data

Data <- data.frame(dfmerge3)
row.names(Data) <- Data$article_id
Data <- Data[,-1]
Data_scaled <- scale(Data) 
Data_scaled  <- data.frame(Data_scaled)

#Determining Optimal Cluster
fviz_nbclust(Data_scaled, kmeans, method = "wss") 
fviz_nbclust(Data_scaled, kmeans, method = "silhouette") 

#visualize kmeans clusters using both k=3 and k=4 for better understanding
k3 <- kmeans(Data_scaled, centers = 2, nstart = 25)
k3
k4 <- kmeans(Data_scaled, centers = 4, nstart = 25)
k4
fviz_cluster(k3, geom = "point", data = Data_scaled, pointsize = 0.2) + ggtitle("k = 2")
fviz_cluster(k4, geom = "point", data = Data_scaled, pointsize = 0.2) + ggtitle("k = 4")

#find means of each cluster
saggregate(Data_scaled, by=list(cluster=k4$cluster), mean)

#summary statistics of each cluster for each of the variables.
res <- cbind(dfmerge3, ClusterId = k4$cluster)
res <- as.data.frame(res)
a <- ggplot(res, aes(x = ClusterId, y = totalunitsold, group = ClusterId, fill = as.factor(ClusterId))) + 
  geom_boxplot(show.legend = FALSE) + theme_minimal() + scale_fill_brewer(palette = "Set2") 
b <- ggplot(res, aes(x = ClusterId, y = max_unitsold_week, group = ClusterId, fill = as.factor(ClusterId))) + 
  geom_boxplot(show.legend = FALSE) + theme_minimal() + scale_fill_brewer(palette = "Set2")
c <- ggplot(res, aes(x = ClusterId, y = avg_quantity_day, group = ClusterId, fill = as.factor(ClusterId))) + 
  geom_boxplot(show.legend = FALSE) + theme_minimal() + scale_fill_brewer(palette = "Set2")
d <- ggplot(res, aes(x = ClusterId, y = transcation_per_article_perday, group = ClusterId, fill = as.factor(ClusterId))) + 
  geom_boxplot(show.legend = FALSE) + theme_minimal() + scale_fill_brewer(palette = "Set2")
grid.arrange(a,b,c,d, ncol = 4)


#exploratory data analysis
#total quantity By day of the week Analysis
ggplot(df2, aes(x = weekday, y = max_unitsold_week, fill = weekday)) +
  geom_bar(stat = "identity") + theme_minimal()  


#total quantity By day of the month Analysis
ggplot(summarise(group_by(retail, day), total_quantity = sum(quantity)/31), aes(x = day, y = total_quantity))+ geom_bar(stat = 'identity', fill = 'Steel Blue') + labs(x = 'day', y = 'total quantity', title = 'total quantity by day of month') + 
  theme_minimal()

#Report: people tend to order in 1st and 28th  of April for most . There is fluctation in sales 
# starting form 3rd of april to 24th of april. But overall starting and  end of the months can be seen as the sale increasing .

 
 