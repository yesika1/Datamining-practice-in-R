# Support Vector Machine SVM
# =======================

# Non probabilistic bynary linear classifier, also for regression (numeric) problems
# Representation of the examples as points in space, where examples of different categories are far away
# gap beteen classes, new examples classify according to distance (side of the gap located).
# hyperline: DEcision boundary, separate two classes, line furth away from data as possible
# support vectors: Training points in the marging lines (boundaries lines from hyperline)
# non- linearity separable data: higher dimension separable
# cost: Soft margin cost function, allows SVM to have soft margins (some examples to be ignore or be place in the wrong place of the margin and dont be part of the classes)
# Cost controls the influence of each individual support vector
# involves trading error penalty for stability. 
# costs allows certain train labels to cross over the soft margin
# gamma parameter: has to deal with non-linear kernel functions 'kernel tricks' in high dimension to pass the line.
# gamma is the free parameter of the Gausian radial basis function.
# small gamma: large variance, large influence of support vector in other class.
# large gamma: small variance, support vectors doesnt not have a wide spread influence
# large gamma leads to high bias and low variance

library(ISLR)
library(e1071)
help('svm')

print(head(iris))
model_svm <- svm(Species ~ . , data=iris )
predict_values <- predict(model_svm, iris[1:4])
summary(model_svm)
table(predict_values,iris[,5])
predict_values 
#              setosa versicolor virginica
#setosa         50          0         0
#versicolor      0         48         2
#virginica       0          2        48


# Tunning: 

#try different combinations of parameter to obtain the lowest error rate
tune_results <- tune(svm, 
                     train.x = iris[1:4], train.y = iris[,5], 
                     kernel= 'radial',
                     ranges = list(cost=c(0.1,1,10), gamma= c(0.5,1,2))
                     )

summary(tune_results)
#- sampling method: 10-fold cross validation
#- best parameters:
#cost gamma
#1   0.5
tune_results$best.model

# doing again with closest values to the previous answer
tune_results2 <- tune(svm, 
                     train.x = iris[1:4], train.y = iris[,5], 
                     kernel= 'radial',
                     ranges = list(cost=c(0.1,1,1.5), gamma= c(0.1,0.5,0.7))
                     )
summary(tune_results2)
#- best parameters:
#cost gamma
#1   0.1

# new model with tuned values
tuned_svm <- svm(Species ~., data = iris, kernel='radial', cost=1, gamma=0.1)
summary(tuned_svm)


#Exercise
loans <- read.csv("~/Dropbox/Yesi/2.UIC/Udemy/R-for-Data-Science-and-Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/loan_data.csv")
View(loans)
str(loans)
summary(loans)

#cleaning data
# convert to categorical
names_categorical <- c('inq.last.6mths',
                       'delinq.2yrs',
                       'pub.rec',
                       'not.fully.paid',
                       'credit.policy')

loans[,names_categorical] <- lapply(loans[,names_categorical], factor)

# EDA 
names(loans)
library(ggplot2)
# histogram of fico scores colored by not.fully.paid 
pl1 <- ggplot(loans,aes(x=fico))  + 
  geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5, position=position_stack(reverse = TRUE)) + 
  scale_fill_manual(values = c('green','red')) + theme_bw()

pl1 <- ggplot(loans,aes(x=fico))  + 
  geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5) + 
  scale_fill_manual(values = c('green','red')) + theme_bw()

table(loans$fico,loans$not.fully.paid)


#barplot of purpose counts, colored by not.fully.paid. 
#Use position=dodge in the geom_bar argument

pl2 <- ggplot(loans,aes(x=factor(purpose)))  + 
  geom_bar(aes(fill=not.fully.paid),position = "dodge") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#scatterplot of fico score versus int.rate. 
pl3 <-  ggplot(loans,aes(int.rate,fico)) +
  geom_point() + 
  theme_bw()

#scatterplot of fico score versus int.rate. color by not.fully.paid
ggplot(loans,aes(int.rate,fico)) +
  geom_point(aes(color=not.fully.paid),alpha=0.3) + 
  theme_bw()

#Modeling
#-------

library(caTools)

set.seed(101)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)

library(e1071)
model <- svm(not.fully.paid ~ .,data=train)
summary(model)
#       cost:  1 
#gamma:  0.01724138 

#predict values
predicted_values <- predict(model,test[1:13])
table(predicted_values,test$not.fully.paid)
#predicted_values    0    1
#0 2413  460 #model classifies everything in one groups
#1    0    0

tune_results <- tune(svm,train.x=not.fully.paid~., data=train,
                     kernel='radial',
                     ranges=list(cost=c(1,10), gamma=c(0.1,1)))

model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted_values <- predict(model,test[1:13])
table(predicted_values,test$not.fully.paid)