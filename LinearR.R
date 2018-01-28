### Linear Regression
#=================
#https://www.youtube.com/watch?v=aq8VU5KLmkY

# Student Performance Data Set from UC Irvine's Machine Learning Repository! 

#libraries
#=========
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot)
library(caTools)


# Read CSV, note the delimiter (sep)
df <- read.csv("~/Dropbox/Yesi/2.UIC/GitHub/R-for-Data-Science-and-Machine-Learning/Machine Learning with R/student-mat.csv", sep=";")
View(df)
head(df)
summary(df)

#correlation
num.cols <-sapply(df, is.numeric)
cor.data <- cor(df[,num.cols])
cor.graph <-corrplot(cor.data,method ='color', type='upper',order="hclust") #Correlation matrix reordered according to the correlation coefficient. This is important to identify the hidden structure and pattern in the matrix. “hclust” for hierarchical clustering order.
#Cleary we have very high correlation between G1, G2, and G3 which makes sense since those are grades


#univariable Analysis
ggplot(df, aes(x=G3)) + geom_histogram(bins=20, alpha=0.5, fill='blue')
#Looks like quite a few students get a zero. This is a good place to ask questions, like are students missing the test? Also why is the mean occurence so high? Is this test curved?


# Expliting data into Train and test data
set.seed(101) # Set a seed for trazability
sample <- sample.split(df$G3,SplitRatio = 0.7) #split data in two sets, SplitRatio: %sample==TRUE
train <- subset(df, sample ==T) #70% data ->train
test <- subset(df, sample ==F) #30% data ->test

# Train and Build model
model1 <- lm(G3 ~ ., data=train)

#Run de model
summary(model1)
# Interpret the model
#residual: dif. between actual point and predicted value
# Estimate: coeficient value of the slope calculated by the regression, cannot compar each value if it isnot normalized data
# Std. Error = measure of variability of the estimate, lower is better (less than coefficient)
# p-value (prob >t): prob. that the variable is not relevant (small good **)
# * = significant level, it is unlikely that not relationship exist.
# *** indicates a low p-value. A low p-value (typically, p < 0.05) is considered statistically significant. The more statistical significance, the more confident you can be when rejecting the null hypothesis that there is no relationship.
# R-squared:Evaluate the goodness of fit of the model (1 the best)
# http://blog.minitab.com/blog/adventures-in-statistics-2/multiple-regession-analysis-use-adjusted-r-squared-and-predicted-r-squared-to-include-the-correct-number-of-variables
# correlation not always imply causation

#residuals
par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))

res <- as.data.frame( residuals(model1) )
pl <- ggplot(res,aes(res)) + geom_histogram(bins=20, alpha=0.5, fill='blue') #if normal dist. then the mean close to zero

# Predictions
G3.predictions <- predict(model1,test)
results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)
head(results)

#taking care of negative values in prediction: grades should be more than 0
# creting to_zero function
to_zero <- function(x){
  if (x<0){
    return (0)
  }else{
    return(x)
  }
}
#applying to_zero function
results$predicted <- sapply(results$predicted,to_zero)


# Checking accuracy of model
#==========
# you can not do an ROC or a confusion matrix for a regression model, thats for classification

# MSE mean squared errors
mse <- mean( (results$actual - results$predicted)^2) #[1] 3.991675

# RME Root mean squared errors
rmse <- mse^0.5 #[1] 1.997918

# SSE Sum of Squared errors
sse <- sum( (results$predicted - results$actual)^2 )
sst <- sum( (mean(df$G3) - results$actual)^2 )
R2 <- 1- sse/sst #[1] 0.8044477 % variance on test data


# Model evaluation: Should we eliminate coefficients that are not that significant to the model in training
# That step you are thinking of usually falls more towards actually deploying your model, 
# where you get rid of features that weren't very useful, 
# but there is no good way to show that because at the point it really depends on 
# what your company/industry values as important features to keep and how fast production responses need to be, 
# how much data you have, etc...

# when would you apply a model that would utilize all the variables as oppose to finding the best model that minimizes BIC and AICc via forward, backward, or mixed selection? 
# Maybe you would use all the variables just to justify to another team/person that you should keep track (or no longer keep track) 
# of that data because they are insignificant/significant. 
# It really depends on the business case and the data, 
# from a pure model building perspective, you should build an efficient model with the important variables only


#Exercise
#  Bike Sharing Demand Kaggle challenge: predict count of bikes
bike <- read.csv("~/Training Exercises/Machine Learning Projects/CSV files for ML Projects/bikeshare.csv")
View(bike)
head(bike)
str(bike)
summary(bike)
names(bike)

#cleaning data
class(bike$datetime) #factor
bike$datetime <- as.POSIXct(bike$datetime)
str(bike$datetime) # POSIXct

# Exploratory Data Analysis
# scatter plot of count vs temp. 
pl1 <- ggplot(bike, aes(x=temp, y=count, color=temp)) +geom_point(alpha=0.3 ) +theme_bw()

#Plot count versus datetime as a scatterplot with a color gradient based on temperature. You'll need to convert the datetime column into POSIXct before plotting.        
pl2 <- ggplot(bike, aes(x=datetime, y=count, color=temp)) +geom_point(alpha=0.3 ) + scale_color_gradient(low='#55D8CE',high='#FF6E2E')

#correlation between temp and count : 0.3944
cor(bike[,c("temp","count")])

num.cols <-sapply(bike, is.numeric)
cor.data <- cor(bike[,num.cols])
cor.graph <-corrplot(cor.data,method ='color', type='upper',order="hclust") #Correlation matrix reordered according to the correlation coefficient. This is important to identify the hidden structure and pattern in the matrix. “hclust” for hierarchical clustering order.

#boxplot with the y axis indicating count and the x axis begin a box for each season.
pl3 <-ggplot(bike, aes(x=factor(season), y=count, color=factor(season))) + geom_boxplot()


# creating hour feature: takes the hour from the datetime column.
bike$hour <- format(bike$datetime,"%H")
class(bike$hour) #character

#subset workingday==1
bikeday1 <- subset(bike,workingday==1)
#Plot count versus hr as a scatterplot with a color gradient based on temperature. You'll need to convert the datetime column into POSIXct before plotting.        
pl4 <- ggplot(bikeday1, aes(x=hour, y=count, color=temp)) +geom_point(alpha=0.3, position=position_jitter(w=1, h=0) ) + scale_color_gradient(low='green',high='orange')

#subset workingday==0
bikeday0 <- subset(bike,workingday==0)
#Plot count versus hr as a scatterplot with a color gradient based on temperature. You'll need to convert the datetime column into POSIXct before plotting.        
pl5 <- ggplot(bikeday0, aes(x=hour, y=count, color=temp)) +geom_point(alpha=0.3, position=position_jitter(w=1, h=0) ) + scale_color_gradient(low='green',high='orange')


# Expliting data into Train and test data
set.seed(101) # Set a seed for trazability
sample <- sample.split(bike$count,SplitRatio = 0.7) #split data in two sets, SplitRatio: %sample==TRUE
train <- subset(bike, sample ==T) #70% data ->train
test <- subset(bike, sample ==F) #30% data ->test

# Train and Build model
temp.model <- lm(count ~ temp, data=train)

#predictions
predict(temp.model,test)
test2 <- data.frame(temp=c(25))
predict(temp.model,test2) #237.0959 

