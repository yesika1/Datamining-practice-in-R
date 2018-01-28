## Logistic Regression 
#=================

# Sigmoid/logistic function
#  teta(x) = 1/(1 +e^-x) 
# take any value and outputs as 0&1. Base off this prob. we assign a class
# We set a cutoff point=0.5, anything below this value goes to class 0, above:class 1.

# Model Evaluation
# confusion matrix (CM) to Evaluate classification models.
# CM is used to describe performance of classification model on test data 
# were the true values are already known.
# terminology, whole numbers:
# TP: True Positives: actual:yes, predicted:yes
# TN: True Negatives: actual:no, predicted:no
# FP: False Positives: actual:no, predicted:yes, Type 1 error: tell a MAN is pregnant
# FN: False Negatives: actual:yes, predicted:no, Type 2 error: tell a PREGNANT with belly woman that is not pregnant

# Rates:
# Accuracy: Overall, how often is it correct? 
# Accuracy = (TP+TN)/Total
# Misclassification Rate (Error Rate): Overall, how often is it wrong? 
# Error Rate = (FP+FN)/total 

#libraries
library(Amelia)
library(ggplot2)
library(dplyr)
library(caTools)
library(ROCR)

# Using Titanic Dataset
df.train <- read.csv("~/Machine Learning with R/titanic_train.csv")
View(df.train)
str(df.train)
names(df.train)

sum(is.na(df.train)) # 177
pl1 <- missmap(df.train, main = "Missing Data Map", legend = T)

#univariable analysis
pl2 <- ggplot(df.train, aes(Survived)) + geom_bar()
pl3 <- ggplot(df.train, aes(Pclass, fill=factor(Pclass))) + geom_bar()
pl4 <- ggplot(df.train, aes(Sex, fill=factor(Sex))) + geom_bar()
pl5 <- ggplot(df.train, aes(SibSp, fill=factor(SibSp))) + geom_bar()

pl6 <- ggplot(df.train, aes(Age)) + geom_histogram(bins=20, alpha=0.5, fill='#55D8CE')
pl7 <- ggplot(df.train, aes(Fare)) + geom_histogram(bins=20, alpha=0.5, fill='#55D8CE')

#bivariable analysis
pl8 <-ggplot(df.train, aes(Pclass,Age)) + geom_boxplot( aes( group=Pclass, fill=factor(Pclass), alpha=0.4))
pl8 + scale_y_continuous( breaks = seq(from=0,to=80,by=2)) 

# Populate Age missing values using mean value of Age by Pclass
imputeAge <- function(age,class){
  newAge <- age
  for (i in 1:length(age)){
    if (is.na(age[i])){
      if (class[i] == 1){
        newAge[i] <- 37
      }else if (class[i] == 2){
        newAge[i] <- 29
      }else{
        newAge[i] <- 24
      }
    }else{
      newAge[i]<-age[i]
    }
  }
  return(newAge)
}

fixed.ages <- imputeAge(df.train$Age,df.train$Pclass)
df.train$Age <-fixed.ages
sum(is.na(df.train)) #0

#removing variables 
# from cabin we could extract "A" and create a new feature
df.train <- select(df.train,-PassengerId,-Ticket,-Cabin,-Name)
varNames <- c('Survived', 'Pclass', 'Parch', 'SibSp')
df.train[,varNames] <- lapply(df.train[,varNames] , factor)
str(df.train)


#training the model
log.model <- glm(formula= Survived ~ ., family = binomial(link = 'logit'), data=df.train)
summary(log.model)

#subsetting
set.seed(101)
split <-sample.split(df.train$Survived, SplitRatio = 0.7)
final.train <-subset(df.train,split==T)
final.test <-subset(df.train,split==F)

final.log.model <- glm(Survived ~ ., family = binomial(link = 'logit'), data=final.train)
summary(log.model)

#PRedictions
fitted.probabilities <- predict(final.log.model,newdata=final.test,type='response')
fitted.results <- ifelse(fitted.probabilities >0.5, 1, 0) # (condition,T,F)
final.test$Suvirvalpredicted <- fitted.results


#misclassification Error
misclassError <- mean(fitted.results != final.test$Survived)
accuracy <- 1-misclassError #0.7985

# Confusion Matrix
table(final.test$Survived,fitted.probabilities>0.5)
#    FALSE TRUE
#0   140   25
#1    29   74

# ROC curve
#http://www.joyofdata.de/blog/illustrated-guide-to-roc-and-auc/
pred<-prediction(fitted.results,final.test$Survived)
perf<-performance(pred,"tpr", "fpr")
plot(perf,col='red', main='ROC curve')


#Exercise
#=======
adult <- read.csv("~/Training Exercises/Machine Learning Projects/CSV files for ML Projects/adult_sal.csv")
View(adult)
head(adult)
str(adult)
summary(adult)
sum(is.na(adult)) #0 However some values are ?

#libraries
library(dplyr)
library(car)
library(Amelia)
library(ggplot2)
library(caTools)

#cleaning data

#droping feature x
adult$X <- NULL
#adult <- select(adult,-X)

# Combining factors: they shouldnt be two digits number
# type_employer reduce factors
table(adult2$type_employer)
adult <- adult %>% mutate(type_employer= recode(type_employer, " c('Without-pay', 'Never-worked')='Unemployed'; c('Self-emp-inc','Self-emp-not-inc')='self-emp'; c('Local-gov','State-gov')='SL-gov' ")) 

# marital reduce factors
table(adult$marital)
adult <- adult %>% mutate(marital= recode(marital, " c('Divorced', 'Separated', 'Widowed')='Not-Married'; c('Married-AF-spouse','Married-civ-spouse', 'Married-spouse-absent')='Married'; ")) 

#countries
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$country <- as.factor(sapply(adult$country,group_country))
table(adult$country)
#rename the attribute from country to region
adult <- rename(adult,region = country) #new=old
str(adult)


#transforming '?' to NA
adult[adult=='?'] <-NA
#adult2[] <- lapply(adult2, gsub, pattern = "?", replacement = NA, fixed = TRUE)
sum(is.na(adult)) #3679
pl1 <- missmap(adult2, main = "Missing Data Map", legend = T, labels)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#dropping NAs
adult <- na.omit(adult)

#removing level ?
table(adult$type_employer) # ?=0
adult$type_employer <- factor(adult$type_employer)
adult$occupation <- factor(adult$occupation)


#EDA
#histogram of ages, colored by income.
pl2 <- ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()
# barplot of region with the fill color defined by income class
pl3 <- ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()
pl3 + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#model

set.seed(101) 
# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE
# Training Data
train = subset(adult, sample == TRUE)
# Testing Data
test = subset(adult, sample == FALSE)

model = glm(income ~ ., family = binomial(logit), data = train)
summary(model)

# step : delete variables that are not significant to the fit
new.model <- step(model)
#AIC criteria
summary(new.model)
#prediction
test$predicted.income = predict(model, newdata=test, type="response")
table(test$income, test$predicted.income > 0.5)
#       FALSE TRUE
#<=50K  6372  548
#>50K    872 1423

#we choose "<50k" as Positive
accuracy.model <-(6372+1423)/(6372+1423+548+872) # 0.8459034
recall.model <- 6732/(6372+548) #0.9728324
precision.model <- 6732/(6372+872) # 0.9293208
# when you calculated Recall  you calculated the True Negative Rate which is TN/(TN+FP) 
#when you calculated Precision you actually calculated the Negative Predictive Value which is TN/(TN+FN)
# Precision is TP/(TP+FP) which is basically how much truth did you get from all your positive readings. 

#R comes with a function called step(). The step() function iteratively tries to remove predictor variables from the model in an attempt to delete variables that do not significantly add to the fit. How does it do this? It uses AIC. 

# it is a good model:
# given the information there is not a real way to get a correct evaluation
# of how good the model is. What we know is the cost associated with accuracy
# precision and recall
# some model you try to max accuracy, while others recall& precission
# Then think about this: if you are trying to detect cancer, what would be more important, reducing type 1 error, or reducing type 2 error? 