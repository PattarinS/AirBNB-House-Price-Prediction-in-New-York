data <- read.csv('AB_NYC_2019.csv', header = TRUE)

#drop last review
drop <- c("last_review")
data = data[,!(names(data) %in% drop)]

summary(data)
data[data == " "] <- NA
summary(data)

#install.packages("DataExplorer","tidyverse")
library(DataExplorer)                       
library(tidyverse)
library(forcats)
data  %>% introduce() 
data %>% plot_missing() 
data %>% profile_missing()

#drop rows, which price=0 and avaiability=0
data <- data[!(data$price == 0 | data$availability_365 == 0),]

#drop columns that no correlation with target
drop <- c('id','name','host_id','host_name','reviews_per_month','neighbourhood')
data = data[,!(names(data) %in% drop)]

#Desity plot for check destitution 
data  %>% plot_density()
data  %>% plot_histogram()

#detect outlier
boxplot(data[5:8])
boxplot(data[2], xlab="latitude")
boxplot(data[3], xlab="longitude")
boxplot(data[9], xlab="availability_365")
boxplot.stats(data$price, coef=2)$out

#Remove outlier
#Setting the bench mark
#price
outliers <- boxplot.stats(data$price)$out
data[which(data$price %in% outliers),]
data <- data[-which(data$price %in% outliers),]
#minimum_nights
outliers2 <- boxplot.stats(data$minimum_nights)$out
data[which(data$minimum_nights %in% outliers2),]
data <- data[-which(data$minimum_nights %in% outliers2),]
#calculated_host_listings_count
outliers3 <- boxplot.stats(data$calculated_host_listings_count)$out
data[which(data$calculated_host_listings_count %in% outliers3),]
data <- data[-which(data$calculated_host_listings_count %in% outliers3),]
#latitude
outliers4 <- boxplot.stats(data$latitude)$out
data[which(data$latitude %in% outliers4),]
data <- data[-which(data$latitude %in% outliers4),]
#longitude
outliers5 <- boxplot.stats(data$longitude)$out
data[which(data$longitude %in% outliers5),]
data <- data[-which(data$longitude %in% outliers5),]
#number of reviews
outliers6 <- boxplot.stats(data$number_of_reviews)$out
data[which(data$number_of_reviews %in% outliers6),]
data <- data[-which(data$number_of_reviews %in% outliers6),]

#Boxplot after remove outlier
boxplot(data[5], xlab="price")
boxplot(data[6], xlab="minimum_nights")
boxplot(data[8], xlab="calculated_host")
boxplot(data[7], xlab="number of reviews")
boxplot(data[2], xlab="latitude")
boxplot(data[3], xlab="longitude")

#Count of categorical Variable-Bar Chart
library('ggplot2')
ggplot(data, aes(neighbourhood_group)) + geom_bar()
ggplot(data, aes(room_type)) + geom_bar()

#Distribution of prices across location- Box Plot
ggplot(data, aes(neighbourhood_group,price, fill=neighbourhood_group)) + geom_boxplot()

#Room types occupied by the neighbourhood_group
ggplot(data, aes(room_type, fill=neighbourhood_group)) + geom_bar(position = 'dodge')

#Desity plot for check destitution 
data  %>% plot_density()
data  %>% plot_histogram()

#Dummy variables
#install.packages('fastDummies')
library('fastDummies')
data_dummy <- dummy_cols(data, select_columns = c('neighbourhood_group', 'room_type'),
                      remove_selected_columns = TRUE)



#Min-max normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# variables
data_dummy<-normalize(data_dummy)


#sort columns
data_dummy <- data_dummy[c(8,9,10,11,12,13,14,1,2,4,5,6,7,3)]
summary(data_dummy)

#Save CSV.file
write.csv(data_dummy,'BNB_EDA.csv')


