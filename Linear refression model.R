#Multiple Linear Regression (MLR)
data = read.csv("BNB_EDA.csv",header = T,stringsAsFactors = T)

#drop index
drop <- c("X")
data = data[,!(names(data) %in% drop)]
str(data)

#respond var= Price
#explanatory var 
#x1=neighbourhood_group_Bronx, x2=neighbourhood_group_Brooklyn, 
#x3=neighbourhood_group_Manhattan, x4=neighbourhood_group_Queens,
#x5=room_type_Entire home/apt, x6=room_type_Private room
#x7=room_type_Shared room
#x8=latitude, x9=longtitude
#x10=minimum_nights, x11=number_of_reviews
#x12=calculated_host_listings_count,
#x13=availability_365

varnames = c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","Price")
colnames(data) = varnames
str(data)

library(GGally)
ggcorr(data, method = c("everything", "pearson")) 
#we want to see high correlation between x and y
#and low correlation between x's

# Variable selection
full_mod =lm(Price ~., data=data)

library(MASS)
step.model =  stepAIC(full_mod, direction = "both", 
                      trace = FALSE)
summary(step.model)

#split data into Training-Testing set
## 90% of the sample size
smp_size <- floor(0.90 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

#Start building model
#Multiple regression model
fit.full=lm(Price ~.,data=train)
summary(fit.full) #We found the result of x4,x7 are NA

fit.full1=lm(Price ~ x1+x2+x3+x5+x6+x8+x9+x10+x11+x12+x13,data=train)
summary(fit.full1)

pred = predict(fit.full1,test)
pred

#Model checking

# Model performance
library(tidyverse)
library(caret)
theme_set(theme_bw())

# (a) Prediction error, RMSE
RMSE(pred, test$Price)
0.123/mean(train$Price) 

# (b) R-square
R2(pred, test$Price)
   
#plot predicted vs. actual values
plot(x=predict(fit.full1,test), y=test$Price,
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values')
#add diagonal line for estimated regression line
abline(a=0, b=1)

par(mfrow = c(2, 2))
plot(fit.full1)
