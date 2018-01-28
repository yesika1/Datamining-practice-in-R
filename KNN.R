# K Nearest Neighbors KNN
#========

# Association with close point to predict the class of a new point
# Predict the class of a given test observation by identify the k observations that are near to it
# Training algorithm: store all data
# prediction algorithm: 
# -calculate distance from x to all points in the data, where x is the new data point
# - sort point in data from closest to farthest to x
# - predict the majority label of the k closest points
# - k: points to look at that are next to the x test point
# - choosing a k will affect the class a new point is assign to.
# Distance metric to select: how to you define matematically the dist to the next point & old training set
# categorical var dont work well with knn
# Knn needs to standardize the vars, the scale really matters

# A standardized variable (sometimes called a z-score or a standard score) is a variable that has been rescaled to have a mean of zero and a standard deviation of one. For a standardized variable, each case’s value on the standardized variable indicates it’s difference from the mean of the original variable in number of standard deviations (of the original variable). For example, a value of 0.5 indicates that the value for that case is half a standard deviation above the mean, while a value of -2 indicates that a case has a value two standard deviations lower than the mean
#  normalization of ratings means adjusting values measured on different scales to a notionally common scale, often prior to averaging.


#Example
#========

library(ISLR)
library(class)
library(ggplot2)

head(Caravan)
str(Caravan)
summary(Caravan)


sum(is.na(Caravan)) #0
#checking variance
var(Caravan$MOSTYPE) # 165.0378
var(Caravan$MAANTHUI) #0.1647078 Different scale!!

# we are going to standardize all the var except the target var
standardized.Caravan <- scale(Caravan[,-86]) #all but not target var
#checking variance in the matrix
var(standardized.Caravan[,1]) # 1
var(standardized.Caravan[,2]) # 1 same scale!!


#target var (purchase) as new vector 
purchase2 <- Caravan[,86]  # adding a nex column
class(purchase2)

#train & test data: simple example
test.index <- 1:1000 #fist rows
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase2[test.index] #just first rows

train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase2[-test.index]

#knn model in training data
set.seed(101)
predicted.modelKnn <- knn(train.data, test.data, train.purchase, k=1) #trainind data without label, test data without label, labels for training data

#output
head(predicted.modelKnn)
misclass.error <- mean(test.purchase != predicted.purchase) #0.119
#Using mean: Because the results are 1s and 0s, so we can take the mean to achieve an accuracy percentage

#choosing a k value: when k gives the minimal misclassification rate
predicted.purchase <- NULL
error.rate <- NULL

for (i in 1:20){
  set.seed(101)
  predicted.purchase <- knn(train.data, test.data, train.purchase, k =i)
  error.rate[i] <- mean(test.purchase != predicted.purchase)
}
print(error.rate)

#visualize K elbow method
k.values <- 1:20
error.df <- data.frame(error.rate, k.values)
ggplot(error.df,aes(k.values, error.rate)) + geom_point() + geom_line(lty='dotted', color='red')
#flatline after 9, where the elbow is. The we choose k=9 as optimal k value

#model with k=9
set.seed(101)
predicted.modelKnn <- knn(train.data, test.data, train.purchase, k=9) #trainind data without label, test data without label, labels for training data
head(predicted.modelKnn)
misclass.error <- mean(test.purchase != predicted.purchase) #0.059


# Exercise
library(ISLR)
str(iris)

# Standardize data
#-----
# Same order of magnitude
standard_features  <- as.data.frame(scale(iris[,c(1:4)]))
standard_features1  <- scale(iris[1:4])
#checking variance of one columnn
var(standard_features[,1])

final.data <- cbind(standard_features,iris[5])

set.seed(101)
library(caTools)
sample <- sample.split(final.data$Species, SplitRatio = .70)
train <- subset(final.data, sample == TRUE)
test <- subset(final.data, sample == FALSE)

# building KNN model with  k=1

library(class)
species_knn <- knn(train[1:4],test[1:4],train$Species,k=1)

print(species_knn)

#misclassification rate
mean(test$Species != species_knn)
#0.04444444

#choosing a k value
#------

# Create a plot of the error (misclassification) rate for k values ranging from 1 to 10.

species_knn <- NULL
error_rate <- NULL

for (i in 1:10){
  set.seed(101)
  species_knn <- knn(train[1:4],test[1:4],train$Species,k=i)
  error_rate[i] <- mean(test$Species != species_knn)
}

library(ggplot2)
k_values <- 1:10
error_df <- data.frame(error_rate,k_values)

pl <- ggplot(error_df,aes(x=k_values,y=error_rate)) + 
  geom_point() + 
  geom_line(lty="dotted",color='red', size=2)
#optimal value between 2-6



