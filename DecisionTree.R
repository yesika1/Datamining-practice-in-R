### Decision Trees
#=================

# Nodes: attribute and split on its levels/values
# Edges: Outcome of a split to the next node
# Root: Node that performs the first split
# leaves: Terminal nodes that predict the outcomes
# proper separation best split, features that split data perfectly.

### Random Forest (RF)
#=================

# Improve performance (predictive accuracy), using multiple trees with a ramdom sample of features per tree
# Bagging reduce variance in ML methods
# RF create an emsamble of decision trees using bootstrap samples (sample with replacement) of the training set
# m= sample of features, p =total features; m=p^(1/2)
# when there is a strong feature, then most trees split there and are highly correlated. We dont want that, dont reduce variance
# By randomly leave features out, RF decorrelates trees, then the avg. process can reduce variance in resulting model
# Final predictive outcome combines the result accros all the trees, in classification: the majority vote of the trees 
# https://stats.stackexchange.com/questions/197827/how-to-interpret-mean-decrease-in-accuracy-and-mean-decrease-gini-in-random-fore

# feature scaling: Tree methods don't generally require it 
# using different number of trees in forests and then compare the performance (overfitting): I don't think this dataset would actually be a good choice unless you do a very wide range of trees, which I have sometimes found to be confusing to beginners

# Example using Rdataset:kyphosis
# ========

#libraries
library(rpart)
library(rpart.plot)
library(randomForest)

str(kyphosis)
table(kyphosis$Number) # 2:10 by 1

#MODEL Decision tree
tree <- rpart(Kyphosis ~ ., method = 'class', data= kyphosis )
printcp(tree)

#plotting tree
plot(tree, uniform=T, main= "Kyphosis tree")
text(tree, use.n = T, all = T)
#nicer plot
prp(tree, box.palette="Blues")

#prediction
tree.preds <- predict(tree,test)


#MODEL Random Forest
rf.model <- randomForest(Kyphosis ~ .,data= kyphosis )
print(rf.model)
rf.model$predicted
rf.model$ntree #500 default
# can check ntrees Vs. misclassification rate to check if increasing trees the model improves
rf.model$confusion
#           absent present class.error
#absent      59       5   0.0781250
#present     13       4   0.7647059


#Exercise: Decision Tree
#========

#ISLR library and check the head of College 

#libraries
library(ISLR)
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)

df<-College
head(df)
str(df)

#EDA
#scatterplot of Grad.Rate versus Room.Board, colored by the Private column.
pl1 <- ggplot(df,aes(Room.Board,Grad.Rate, color=Private)) + geom_point(size=2, alpha=0.5)
#  histogram of full time undergrad students, color by Private
table(df$Private) 
# No Yes 
# 212 565 

pl2 <- ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50, alpha=0.5)
ggplot(df,aes(F.Undergrad))+geom_histogram(aes(fill=Private),bins = 50,color='black',alpha=0.5,position=position_stack(reverse = TRUE))
#not right, then relevel to start with the highest level
df$Private <- relevel(df$Private, "Yes")
pl2 <- ggplot(df, aes(F.Undergrad)) +geom_histogram(aes(fill = Private),
                 bins = 50,color = 'black',alpha = 0.5) 


#histogram of Grad.Rate colored by Private
pl3 <- ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',bins=50)
#cleaning: College had a Graduation Rate of above 100%
df100 <- subset(df,Grad.Rate > 100) # Cazenovia College
df['Cazenovia College','Grad.Rate'] <- 100 #changing value to 100

#Train & Test Split
set.seed(101) 
sample = sample.split(df$Private, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

#Decision Tree
tree <- rpart(Private ~.,method='class',data = train)
summary(tree)

#predict
tree.preds <- predict(tree,test)
head(tree.preds) # get back prob. that model gives yes vs. no


#Turning these two columns into one column to match the original Yes/No Label for a Private column.
tree.preds <- as.data.frame(tree.preds)

joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return("No")
  }
}

tree.preds$Private <- sapply(tree.preds$Yes,joiner) #add new col with rule
head(tree.preds)

#confusion matrix
table(tree.preds$Private,test$Private)

#ploting tree
prp(tree, box.palette="Greens")


#Exercise: Random Forest
#========

#same df, libraries:
library(randomForest)

#model in train data
rf.model <- randomForest(Private ~ . , data = train,importance = TRUE)

#confusion matrix on training
rf.model$confusion
#less class error in 'yes', which means that its easier to predict this level, also makes sense bc there is more data for private(yes), than public in the dataset.

#importance: based on the Gini impurity index values of info when spliting
rf.model$importance

# predict on test data
p <- predict(rf.model,test)

#confusion matrix on testing
table(p,test$Private)

