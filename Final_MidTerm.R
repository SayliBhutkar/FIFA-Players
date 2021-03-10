install.packages("xlsx")
library(xlsx)
library(dplyr)

#Cleaning the dataset
data1 <- read.csv('C:/Users/gadep/OneDrive/Desktop/ADS/CompleteDataset.csv')
data2 <- read.csv('C:/Users/gadep/OneDrive/Desktop/ADS/fifa19.csv')


m=merge(data1,data2,by="ID")

#data3=merge(data1,data2,by="ID")
finaldata <- m [c(0:11,14:75,86)]
write.csv(finaldata, file="NewADS.csv")

data3 <- read.csv("C:\\Users\\sayli\\OneDrive\\Desktop\\WithNA.csv",1)

#data3 <- read.csv("C:/Users/arroh/Desktop/Analytics Workshop 2_25/NewFifawowie.csv")

#DatatypeIdentification

str(data3)

# #Visualizations
# install.packages("rsample")
# install.packages("randomForest")
# install.packages("caret")
# install.packages("dplyr") 
# install.packages("tidyverse", dependencies=TRUE)
# 
# library(tidyr)
# library(ggplot2)
# library(tidyverse)
# library(lubridate)
# 
# 
# #distribution in histogram based on age
 data4 <- read.csv('C:/Users/gadep/OneDrive/Desktop/ADS/NewFifa1.csv')
# g_age <- ggplot(data = data4, aes(data4$Age))
# g_age + 
#   geom_histogram(col="orange", aes(fill = ..count..)) + ggtitle("Distribution based on Age")
# 
# #distribution in histogram based on Value
# data4 <- read.csv('C:/Users/gadep/OneDrive/Desktop/ADS/NewFifa1.csv')
# g_Value <- ggplot(data = data4, aes(Value))
# g_Value + 
#   geom_histogram(col="orange", aes(fill = ..count..)) + ggtitle("Distribution based on Value")
# 
# #distribution on Position and Age
# data4 <- read.csv('C:/Users/gadep/OneDrive/Desktop/ADS/NewFifa1.csv')
# g_age <- ggplot(data = data5, aes(data5$Age))
# g_age + geom_density(col="orange", aes(fill = Preferred.Positions), alpha=0.5) + facet_grid(.~Preferred.Positions) + ggtitle("Distribution based on Age and Position")


#Divide FIFA 18-19 into training and test datasets (80%/20%)
set.seed(1)
row.number <- sample(1:nrow(data3), 0.8*nrow(data3))
train = data3[row.number,]
test = data3[-row.number,]
dim(train)
dim(test)


str(data3)
# Dimension Reduction

# Checking for correlation
# Correlation heatmap
cormat <- round(cor(data3),2)
cormat
library(reshape2)
melted_cormat <- melt(cormat)

library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

dim(train)
# Removing highly correlated variables
tmp<-cor(train[1:63])
tmp[upper.tri(tmp)] <- 0
diag(tmp)<-0
trainnew <- train[,!apply(tmp,2,function(x) any(x > 0.80))]
str(trainnew)

# Model Creation with uncorrelated variables
LinearRegression1 = lm(Value ~ . ,data = trainnew )
summary(LinearRegression1)
# Still the same very low R square value
testfit1 <- predict(LinearRegression1,test)
testfit2<-predict(LinearRegression1,trainnew)
rmse(testfit2,trainnew$Value)
rmse(testfit1,test$Value)

LinearRegression2 = lm(Value ~ . ,data = train )
summary(LinearRegression2)
testfit3 <- predict(LinearRegression2,train)
LinearRegression2 = lm(Value ~ . ,data = test)
testfit4<-predict(LinearRegression2,test)

#R Square Error
rr <- train$Value - predict(LinearRegression2)
rr

r_square_rf <- 1 - var(rr)/var(train$Value )
options(digit = 2)
c("R Square is", format(round(r_square_rf,2), nsmall = 2))


#rmse value
rmse(testfit3,train$Value)
rmse(testfit4,test$Value)

#Accuracy Value
install.packages("forecast")
library(Metrics)
library(forecast)
Prediction <- predict.lm(object = LinearRegression2,newdata = test)
accuracy( Prediction,test$Value)


Prediction <- predict.lm(object = LinearRegression2,newdata = train)
accuracy(Prediction,train$Value)


# library(Metrics)
# options(scipen = 999, digits = 3) #  use options() to ensure numbers are not displayed in scientific notation.
# accuracy(testfit1,test$Value)
# rmse(testfit1,test$Value)
# rmse(testfit2,trainnew$Value)
# # Reducing variables based on p values, taking only the variables that are significant
# trainnew<-trainnew[,c(1,2,4,5,8,12,14)]
# LinearRegression2 = lm(Value ~ . ,data = train )
# summary(LinearRegression2)
# # The R squared value is very low
# 
# testfit2 <- predict(LinearRegression2,test)
# 
# library(Metrics)
# options(scipen = 999, digits = 3) #  use options() to ensure numbers are not displayed in scientific notation.
# accuracy(testfit,test$Value.y)
# # Still the accuracy is zero
# rmse(testfit2,test$Value.y)


#Decision tree and prediction
#Y1 prediction
library(rpart)
model_tree= rpart(Value ~ . ,data = train)
p= predict(model_tree,test)


rmse(p,test$Value.y)

#Comparison of the Predicted test values
table(p,test[,2])
p
test[,2]
summary(model_tree)

plot(model_tree,uniform = TRUE,main = "Regression Tree")
text(model_tree,use.n = TRUE , all = TRUE ,cex =.8)

error <- test$Value - p
head(test$Value)
head(p)
head(error)
rmse <- sqrt(mean((error)^2))
library(Metrics)
rmse(test$Value,p)

model_tree= rpart(Value ~ . ,data = test)
p= predict(model_tree,test)


rmse(p,test$Value.y)
#R square

rr <- train$Value - predict(model_tree)
rr

r_square_rf <- 1 - var(rr)/var(train$Value )
options(digit = 2)
c("R Square is", format(round(r_square_rf,2), nsmall = 2))



#Random Forest

install.packages("rsample")
install.packages("randomForest")
install.packages("caret")
library(randomForest)
library("rsample")
library(tidyr)
dim(data3)
names(data3)
class(data3)
str(data3)
summary(data3)
summary(train)



fit.rf <- randomForest(formula = Value ~., data = train)
pred.rf <- predict(fit.rf, test)


rmse.rf <- sqrt(mean(((pred.rf) - test$Value)^2))
(rmse.rf)
c(RMSE = rmse.rf, pseudoR2 = mean(fit.rf$rsq))
plot(pred.rf,test$Value, xlab = "Error", ylab = "Value", pch = 3)


#Random forest prediction
library(Metrics)
rmse(test$Value,p)

summary(randomForest)

#R Square Error
rr <- train$Value - predict(fit.rf)
rr

r_square_rf <- 1 - var(rr)/var(train$Value )
options(digit = 2)
c("R Square is", format(round(r_square_rf,2), nsmall = 2))

#xgboost
install.packages("xgboost")
install.packages("Metrics")

library(rJava)
library(xlsx)
library(xgboost)
library(Metrics)

data4 <- read.csv('C:/Users/gadep/OneDrive/Desktop/ADS/NewFifa1.csv')
data <- data.frame(data4)

nrow(data)
set.seed(71)
test_sub <- sample(nrow(data), (1/9)*nrow(data))
train_data <- data[-test_sub,]
test_data <- data[test_sub,]
train_data

train_matrix <- xgb.DMatrix(data = as.matrix(train_data[!names(train_data) %in% c("Value")]), label = train_data$Value)
data.xgb = xgboost(data=train_matrix, max_depth=3, eta = 0.2, nthread=3, nrounds=40, lambda=0
                   , objective="reg:linear")
predict.xgb <- predict(data.xgb, data.matrix(subset(test_data,select=-c(Value))))
summary(predict.xgb)
rmse.xgb <- rmse(as.matrix(test_data["Value"]), as.matrix(predict.xgb))
rmse.xgb


table(data$Age)
mean(data$Age)

#Graph to show Comparing Market Value of Players with Overall Rating')

ggplot(data3,aes(x=Overall.x,y=Value)) + geom_point(alpha=0.3) + 
  labs(x='Overall Rating',y='Value',title='Comparing Market Value of Players with Overall Rating')


